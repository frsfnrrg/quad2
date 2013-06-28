(ns quad2.constants
  (:import (java.awt Color)))

(def board-size "Size of the board" 15)
(def blockers "Initial number of blockers owned" 10)
(def square-empty "Empty square" -1)
(def square-blocker "Square with a blocker on it" -2)
(def square-piece-colors "Colors for the pieces corresponding to each player." (list (Color. 0 255 0) (Color. 255 120 0) (Color. 255 255 0) (Color. 0 55 255) (Color. 0 255 255) (Color. 255 0 255)))
(def number-of-players "Number of players in the game. First is human, rest are AI." 4)