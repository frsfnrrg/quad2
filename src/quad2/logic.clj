(ns quad2.logic
  (:use quad2.constants)
  (:use quad2.util))

(let [ updater-thread (ref nil)
       move-vec (ref (vector))
       pieces-vec (ref (fill-vector
                         (fn [] (vector))
                         number-of-players))
       hotness-board (ref (fill-vector
                            (fn [] (fill-vector
                                     (fn [] (fill-vector
                                              (fn [] 0) number-of-players))
                                     board-size))
                            board-size))]

  (defn setup-ai "Initializes the AI state." []
    (let [mv (vector)
          pv (fill-vector (fn [] (vector)) number-of-players)
          hb (fill-vector (fn [] (fill-vector (fn [] (fill-vector (fn [] 0) number-of-players)) board-size)) board-size)]
      (dosync
        (ref-set move-vec mv)
        (ref-set pieces-vec pv)
        (ref-set hotness-board hb))))
  
  (defn- hbdo [loc player func]
    (dosync (ref-set hotness-board  
              (vector-update @hotness-board (first loc)
                (fn [column] (vector-update column (second loc)
                               (fn [cell] (vector-update cell player func))))))))
  
  (defn- promote [loc player val] 
    (hbdo loc player (fn [v] (+ v val))))
  
  ;; we fill with -1 to distinguish between empty, null ranked and taken squares,
  ;; so as not to confuse the (get-highest-ranked-pos ...)
  (defn- clear [loc] 
    (dosync (ref-set hotness-board  
              (vector-update @hotness-board (first loc)
                (fn [column] (vector-update column (second loc)
                               (fn [cell] (fill-vector (fn [] -1) (count cell)))))))))
  
  (defn- inbounds? [p]
    (let [x (first p)
          y (second p)]
      (and (>= x 0) (< x board-size)
        (>= y 0) (< y board-size))))
  
  (defn- analyze [cmove]
    (let [pos (rest (first cmove))
          movetype (first (first cmove))
          player (second cmove)
          board (nth cmove 2)]
      (if (= movetype :blocker)
        (clear pos)
        
        (do 
          ;; piece analysis
          
          (doseq [i (get @pieces-vec player)]
            (let [rdf (rot90 (map - i pos))
                  pairs-k [[(map + pos rdf) (map + i rdf)]
                           [(map - pos rdf) (map - i rdf)]]
                  
                  pairs (if (every? even? rdf)
                          (let [mid (map (fn [a b] (/ (+ a b) 2)) i pos)
                                dem (map (fn [k] (/ k 2)) rdf)]
                            (conj pairs-k [(map + mid dem) (map - mid dem)]))
                          pairs-k)
                  ]
              (doseq [pair pairs]
                (let [f (get pair 0)
                      vf (getdxy board f)
                      l (get pair 1)
                      vl (getdxy board l)]
                  (when (and (inbounds? f) (inbounds? l))
                    (cond (and (= player vf) (= square-empty vl))
                      (promote l player 100)
                      (and (= player vl) (= square-empty vf))
                      (promote f player 100)
                      (= square-empty vf vl)
                      (do 
                        (promote f player 2)
                        (promote l player 2))))))))
          
          (clear pos)
          
          (dosync (ref-set pieces-vec
                    (vector-update @pieces-vec player
                      (fn [pieces] (conj pieces pos)))))))
      )
    )
  
  ;; maybe: once that completes, let the updater-thread do look-ahead based move ranking. Or sleep. Sleeeep...
  (defn update-ai "brings in the move the player makes, and updates the model given" [move board player]
    
    ;; add to move-list
    (dosync (ref-set move-vec (conj @move-vec (list move player board))))
    
    (if (nil? @updater-thread)
      (dosync (ref-set updater-thread
                (start-thread
                  (while (seq @move-vec)
                    (analyze (get @move-vec 0))
                    (dosync (ref-set move-vec (subvec @move-vec 1))))
                  (dosync (ref-set updater-thread nil)))))))
  
  ;; ai-move waits until completion of update-ai thread. 
  (defn ai-move "Returns a good move for an AI, as a
 list containing (blocker-move blocker-move
 blocker-move ... piece-move)." [board player blockers-left]
    (busy-doing-important-stuff 0.8)
    
    (when-not (nil? @updater-thread)
      (.join @updater-thread))
    
    ;; step one: check for winning moves
    (let [k (get-large-value-pos @hotness-board 100)]
      (if (not (empty? k))
        (let [self (filter (fn [f] (= (second f) player)) k)]
          (if (not (empty? self))
            (let [move (first self)]
              (list (list :piece (nth move 2) (nth move 3))))
            ;; take the first one as the piece, rest as blockers
            (do
              (defn t [label p]
                (list label (nth p 2) (nth p 3)))
              
              (reverse (cons (t :piece (first k))
                         (map (partial t :blocker) (take blockers-left (rest k))))))))
        ;; step two: if self not placed, then take the best (sum of player-hotnesses) ranked square.
        (list (cons :piece (get-highest-ranked-pos @hotness-board)))))
    
    )
  
  ) ; aimove let


;; winning check: has the game been won by the _current_ player?.

;; q? can we get this down to log(n) or nlog(n) or even n?

;; n2 is easy: a-b vector, guess next two positions at right angles, if they
;; contain something, continue

;; should this return the 4 winning squares? Preferably. Use (:w-won (a b c d)), (:w-tie) (:w-nil)
(defn win-check "Returns one of <b>w-won</b>,<b>w-tie</b>,<b>w-nil</b> depending on board state at end of a player's turn."
  [board player move]
  ;; best way: generate this lazily, then keep on streaming the check through in triangle (greedy early) form 12 13 23 14 24 34 ...
  (defn scan-board "Returns a list of pairs of integer coordinates" [board]
    ((fn [x y build]
       (let [ret (if (= player (getxy board x y))
                   (cons (list x y) build)
                   build)]
         (cond (= x (- board-size 1))
           (recur 0 (inc y) ret)
           (= y (- board-size 1))
            ret
            :else
            (recur (inc x) y ret))))
       0 0 (list)))
  
  (defn win-squares "returns true if the this segment is a side of a square of pieces of this piece" [pair]
    (let [a (first pair)
          b (second pair)
          diff (list (- (second b) (second a)) (- (first a) (first b)))]
      (let [ap (map + a diff)
            bp (map + b diff)
            am (map - a diff)
            bm (map - b diff)]
        (cond (= player (getdxy board ap) (getdxy board bp))
          (list a b ap bp)
          (= player (getdxy board am) (getdxy board bm))
          (list a b am bm)
          :else
          false))))

  (let [r (or-coll (map win-squares (combinations2 (scan-board board))))]
    (if r
        (cons :w-won r)
        (if (empty? (get-empty-squares board))
          (list :w-tie)
          (list :w-nil)))))


