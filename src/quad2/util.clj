(ns quad2.util
  (:use quad2.constants))

;; board - vector of columns; board@x is the xth column; board@x@y a cell 
(defn make-board "Create a board: (square Vector array)" [] 
  (defn mbx [left x]
    (if (> left 0)
        (recur (- left 1) (conj x square-empty))
        x))
  (defn mbi [left r]
    (if (> left 0) 
       (recur (- left 1) (conj r (mbx board-size (vector))))
       r))
   (mbi board-size (vector)))
 
(defn clickableButton
  "Generates a button, that, when clicked, displays a popup with a message"
  [text displayed]
  (let [b (javax.swing.JButton. text)]
    (.addActionListener b
     (proxy [java.awt.event.ActionListener] []
       (actionPerformed [evt]
         (javax.swing.JOptionPane/showMessageDialog  nil,
            displayed))))
    b))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; funky bug: in one situation, when we act on an array of arrays, 
;; the main (immutable) source object changes value, despite this being the only thread
;; working on it. Now, we pass the source around, and all is fine! 

;; Q: was it a Persistant Vector glitch??
(defn mapvc "maps a function onto a single vector, passing in the location int as first arg" [func vec]
  (let [length (count vec)]
    (defn subrec [nv p source]
      (if (< p length)
        (recur (conj nv (func p (get source p))) (+ p 1) source)
        nv))
    (subrec (vector) 0 vec)))

(defn mapc "maps a function onto a single list, passing in the location int as first arg" [func list]
  (let [length (count vec)]
    (defn subrec [nv p source]
      (if (< p length)
        (recur (cons (func p (get source p)) nv) (+ p 1) source)
        nv))
    (subrec (list) 0 vec)))

(defn subtract-at "Decrease the value in a vector at position c by one." [vec c]
  (mapvc (fn [i v]
           (if (= i c)
             (- v 1)
             v))
    vec))

(defn getxy "Get value of 2d vector array at position (x, y)" [arr x y]
  (get (get arr x) y))
(defn getdxy [b pos]
  (getxy b (first pos) (second pos)))


(defn log
  "Takes, prints, and returns the single parameter."
  [v]
  (println v)
  v)



(defn copy-vector
  "Fills a vector of size n with an object.
 Does <i>not</i> deepcopy"
  [val size]
  ;; use alambda (w/ self call)
  ;; ex. (defn cv [val size] ((alambda [c b] (if (>= c size) b (self (+ c 1) (conj b (.clone val)))))))
  (defn yuck [c b]
    (if (>=  c size)
        b
        (recur (+ c 1) (conj b val))))
  (yuck 0 (vector)))

(defn board-change
  "change the value of one square on the board"
  [old x y val]
  (mapvc
    (fn [p vec]
      (if (= p x)
        (mapvc
          (fn [q item]
            (if (= q y)
              val
              item))
          vec)
        vec))
    old))

(defn list-merge
  "Merge two lists. Not in a specific order."
  [a b]
  (if (empty? b)
      a
      (recur (cons (first b) a) (rest b))))

(defn combinations2
  "Returns a list of combinations of an iterable"
  [iter]
  (if (>= (count iter) 2)
    (do
      (defn subcombs [count cap arr itail pair]
        (if (or (empty? itail) (>= count cap))
          arr
          (recur (inc count) cap (cons (list pair (first itail)) arr) (rest itail) pair)))
      (reduce list-merge (mapvc
                           (fn [c pair] (subcombs 0 c (list) iter pair))
                           (vec iter))))
    '()))

(defn get-empty-squares
  "Returns a list of empty squares on the board."
  [board]
  ;; replaces filled squares with nils; flattens, filters out nils.
  ;; so what if it is redundant
  (reduce list-merge (map
                       (fn [col] (filter
                                   (fn [p] (not (nil? p)))
                                   col)) 
                       (mapvc
                         (fn [i col] (mapvc
                                       (fn [j cell] (if
                                                      (= cell square-empty)
                                                      (list i j) nil))
                                       col))
                         board))))


(defn busy-doing-important-stuff
  "Feign activity"
  [sec]
  (println "Commencing...")
  (dotimes [k (int sec)]
    (println "Calculating...")
    (Thread/sleep 1000))
  (Thread/sleep (int (* 1000 (mod sec 1.0))))
  (println "Done!"))

(defn fill-vector [thunk size]
  ((fn [c b]
    (if (>=  c size)
        b
        (recur (+ c 1) (conj b (thunk)))))  0 (vector)))

(defn fill-list [thunk size]
  ((fn [c b]
    (if (>= c size)
        b
        (recur (+ c 1) (cons (thunk) b))))  0 (list)))

(defn vector-update [v pos func]
  (mapvc (fn [c val] (if (= c pos) (func val) val)) v))

(defn list-update [v pos func]
  (mapc (fn [c val] (if (= c pos) (func val) val)) v))

(defn merge-two
  "Merges two collections. Order may not be preserved."
  [alpha beta]
  (if (coll? alpha)
      (if (coll? beta)
          (if (empty? beta)
            alpha
            (apply conj alpha beta))
          (conj alpha beta))
      (if (coll? beta)
          (conj beta alpha)
          (list alpha beta))))

(defn flatten-level [structure level]
  (cond (<= level 0)
      structure
      (coll? structure)
      (reduce merge-two
        (map (fn [s] (flatten-level s (dec level)))
          structure))
      :else
      structure))

(defn get-large-value-pos [va min] 
  ;; iterate through all leaves
  ;; or use (partition 4 (flatten ...))
  (flatten-level
    (mapvc 
      (fn [x col]
        (mapvc 
          (fn [y cell]
            (filter (fn [kc] (> (first kc) min))
              (mapvc (fn [pl val] (list val pl x y)) cell)))
          col))
      va)
    2))

(defn get-highest-ranked-pos [va]
  (rand-nth
    ((fn [b x y bestv bestp]
       (cond (= y board-size)
         (recur b (inc x) 0 bestv bestp)
         (= x board-size)
         bestp
         (> (getxy b x y) bestv)
         (recur b x (inc y) (getxy b x y) (vector (list x y)))
         (= (getxy b x y) bestv)
         (recur b x (inc y) bestv (conj bestp (list x y)))
         :else
         (recur b x (inc y) bestv bestp)))
      (mapv 
        (fn [col]
          (mapv 
            (fn [cell]
              (apply + cell)) 
            col))
        va)
      0
      0
      -1
      nil)))


(defn rot90 "Rotates a vector by +90 degrees." [point]
  (list (- (second point)) (first point)))

(defn or-coll "Returns the first truthy value in the collection, else false." [coll]
  ;; aif
  (cond
      (empty? coll)
      false
      (first coll)
      (first coll)
      :else
      (recur (rest coll))))

(defmacro start-thread
  "Starts a thread that does the operations
 specified in exprs. The thread object is bound to selfname.
 (proxy-super ...) still works.

 Example:

 (start-thread thread
   (busy-doing-important-stuff 2)
   (println \"Running under\" thread)
   (busy-doing-important-stuff 5)
 )
" 
  [& exprs]
  `(doto (proxy [java.lang.Thread] []
                    (run [] ~@exprs))
     (.start)))