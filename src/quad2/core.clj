(ns quad2.core
  (:gen-class)
  (:use quad2.constants)
  (:use quad2.util)
  (:use quad2.logic))

; CONSTANTS

; double vector of button objects that can be repainted
(def buttons nil)
(def blocker-count-display (ref nil))

; current board state
(def current-board (ref nil))
(def current-player (ref nil))
(def placing-blockers (ref nil))
(def blocker-count (ref nil))
(def human-turn (ref nil))
(def game-running (ref nil))
(def winning-squares (ref nil))

(defn- update-blocker-display []
  (let [c (get @blocker-count 0)]
    (if (= c 1)
      (.setText @blocker-count-display "1 Blocker")
      (.setText @blocker-count-display (str c " Blockers")))))

(defn winning-button? [x y]
  (some (fn [a] (and (= (first a) x) (= (second a) y))) @winning-squares))

(defn get-game "Gets a game update function" []

  (def buttons (vector))
  
  (defn setbxy! [x y v]
    (dosync (ref-set current-board (board-change @current-board x y v))
    nil
    ))
  
  (defn repaint-all-buttons []
    (doseq [x (range 0 board-size) y (range 0 board-size)]
      (.repaint (get buttons (+ (* y board-size) x)))))
  
  (defn reset-game []
    (dosync 
      (ref-set current-board (make-board))
      (ref-set current-player 0)
      (ref-set placing-blockers false)
      (ref-set blocker-count (copy-vector blockers number-of-players))
      (ref-set human-turn true)
      (ref-set game-running true)
      (ref-set winning-squares (list))))
  
  (defn update-game "Updates the game with given input, updates the appropriate buttons"
    [input]
    (if (or (empty? input) (not @game-running))
      "done"
      (let [move (first input)
            t (first move)
            x (second move)
            y (nth move 2)]
        ; move is: (typeflag x y)
        
        (when (= (getxy @current-board x y) square-empty)
          (let [previous-player @current-player]
            
            (if (= t :blocker)
              (if (> (get @blocker-count @current-player) 0)
                (do (setbxy! x y square-blocker)
                  (dosync (ref-set blocker-count (subtract-at @blocker-count @current-player)))
                  (if (= 0 @current-player)
                    (update-blocker-display)))
                (println "No blockers left..."))
              (do (setbxy! x y @current-player)
                (dosync (ref-set current-player (mod (+ @current-player 1) number-of-players)))))
            
            (.repaint (get buttons (+ (* y board-size) x)))
            
            (update-ai move @current-board previous-player)
            
            (when (= t :piece)
              (let [r (win-check @current-board previous-player input)]
                (case (first r)
                  :w-won
                  (do
                    (dosync (ref-set game-running false))
                    (println "Player" previous-player "Won!")
                    (dosync (ref-set winning-squares (rest r)))
                    (repaint-all-buttons))
                  :w-tie
                  (do
                    (dosync (ref-set game-running false))
                    (println "Tie Game!")
                    (dosync (ref-set winning-squares (list))
                    (repaint-all-buttons)))
                  :w-nil
                  (when-not (= 0 @current-player) 
                    ; switch to ai
                    (dosync (ref-set human-turn false))
                    (start-thread
                      (let [move-maker @current-player]
                        (update-game (ai-move @current-board @current-player (get @blocker-count @current-player)))
                        (when (= move-maker (- number-of-players 1))
                          (dosync (ref-set human-turn true))))))
                  )))))
        (recur (rest input)))))
  
  (reset-game)
  (setup-ai)
  
  (defn restart []
    (reset-game)
    (setup-ai)
    (repaint-all-buttons)
    (update-blocker-display))
  
  (list update-game restart))





;
; Basic game plan: we have a setting oriented sidebar;
; A toplevel menu system;
; and a center right panel for gameplay.
;
;   =============
;   X|. . . . . .
;   X|. . . . . .
;   X|. . . . . .
;   X|. . . . . .
;   X|. . . . . .
;   X|. . . . . .
;

(defn grayed-out "Grays out a color" [color]
  (defn scale [comp]
    (int (/ (+ comp 255) 2)))
  (java.awt.Color. (scale (.getRed color)) (scale (.getGreen color)) (scale (.getBlue color))))

(defn mbutton "returns an MButton" [gamef x y]
  (let [button (proxy [javax.swing.JButton] []
           (paint [graphics]
              (let [state (getxy @current-board x y)
                    color (cond (= state square-empty)
                            java.awt.Color/WHITE
                            (= state square-blocker)
                            java.awt.Color/BLACK
                            (= state nil)
                            java.awt.Color/PINK
                            :else
                            (nth square-piece-colors state))]
                (if (or @game-running (winning-button? x y))
                    (.setColor graphics color)
                    (.setColor graphics (grayed-out color)))
                (doto graphics
                  (.fillRect 0 0 50 50))))
           (.toString [] (pr-str "<Square(" x "," y ")>"))
         )]
  (doto button
    (.setMinimumSize (java.awt.Dimension. 25 25))
    (.setPreferredSize (java.awt.Dimension. 50 50))
    (.setMaximumSize (java.awt.Dimension. 50 50))
    (.addActionListener
      (proxy [java.awt.event.ActionListener] []
           (actionPerformed [evt]
             (if (and @human-turn @game-running)
               (if @placing-blockers
                 (gamef (list (list :blocker x y)))
                 (gamef (list (list :piece x y)))))))))))

(def run nil)
(def close nil)

(let[] 
  (defn run-local "Runs program" []
    (let [window (javax.swing.JFrame. "Quad")
          game (get-game)
          menubar (javax.swing.JMenuBar.)
          cp (.getContentPane window)
          pain-side (javax.swing.JPanel.)
          pain-main (javax.swing.JPanel.) ; proxy, give a colored background, & fix size?
          button-piece (javax.swing.JRadioButton. "Piece")
          button-block (javax.swing.JRadioButton. "Blocker")
          button-restart (javax.swing.JButton. "Restart")
          menubutton-restart (javax.swing.JMenuItem. "Restart")
          bgroup (javax.swing.ButtonGroup.)
          restarter (proxy [java.awt.event.ActionListener] []
                      (actionPerformed [evt]
                        ((second game))))
          blocker-count (javax.swing.JLabel. (str blockers " Blockers") javax.swing.SwingConstants/RIGHT)]
      (doto window
        (.setJMenuBar menubar))
      (.setSelected button-piece true)
      ;; use a macro?? (dotoall (fn [a b] (.add ... )) [button-block true] [button-piece false])  
      (.addChangeListener button-block
        (proxy [javax.swing.event.ChangeListener] []
          (stateChanged [evt]
            (when (.isSelected  (.getSource evt))
              (dosync (ref-set placing-blockers true))))))
      (.addChangeListener button-piece
        (proxy [javax.swing.event.ChangeListener] []
          (stateChanged [evt]
            (when (.isSelected  (.getSource evt))
              (dosync (ref-set placing-blockers false))))))
      (.addActionListener button-restart restarter)
      (.addActionListener menubutton-restart restarter)
        
      (doto bgroup
        (.add button-block)
        (.add button-piece)
        )
      (dosync (ref-set blocker-count-display blocker-count))

      (doto pain-side
        (.setLayout (javax.swing.BoxLayout. pain-side javax.swing.BoxLayout/Y_AXIS))
        (.add (javax.swing.Box/createVerticalGlue))
        (.add blocker-count)
        (.add (javax.swing.Box/createVerticalStrut 4))
        (.add button-piece)
        (.add button-block)
        (.add button-restart)
        (.add (javax.swing.Box/createVerticalGlue)))
      (doto pain-main 
        (.setLayout (java.awt.GridLayout. board-size board-size 3 3)))
      (doseq [x (range 0 board-size) y (range 0 board-size)]
        (let [b (mbutton (first game) y x)]
          (.add pain-main b)
          (.repaint b)
          (def buttons (conj buttons b))))
      
      (doto menubar 
        (.add  
          (doto (javax.swing.JMenu. "Game")
             (.add menubutton-restart))))
      
      (doto cp
        (.setLayout (javax.swing.BoxLayout. cp javax.swing.BoxLayout/X_AXIS))
        (.add pain-side)
        (.add pain-main)
        (.add (javax.swing.Box/createHorizontalGlue)))
      (doto window
        (.setDefaultCloseOperation javax.swing.WindowConstants/DISPOSE_ON_CLOSE)
        (.pack)
        (.setResizable true)
        (.setVisible true))
      
      (fn []
        (let [wasopen (.isDisplayable window)]
          (.dispatchEvent window (java.awt.event.WindowEvent. window java.awt.event.WindowEvent/WINDOW_CLOSING)) wasopen))
      ))
  
  (def close (fn [] false))
  (def run (fn [] (let [] (close) (def close (run-local)) nil)) )
)

(defn -main "Jar entry point" [& args]
  (run))
