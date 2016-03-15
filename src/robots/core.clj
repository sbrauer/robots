(ns robots.core
  (:gen-class))

(def ^:const cols 59)
(def ^:const rows 22)
(def ^:const player-char \@)
(def ^:const robot-char \+)
(def ^:const pile-char \*)

(defn pad
  [coll n padding]
  (take n (concat coll (repeat padding))))

; Coords are 2-item vectors [x y] (where top-left is [0 0])
; A "board" is a map with the coords of the :player, :robots, and :piles
; A "grid" is a vector of vectors of characters representing the printable board.

(defn empty-grid
  []
  (vec (repeat rows (vec (repeat cols  \space)))))

(defn add-char-to-grid
  [grid [x y] ch]
  (assoc grid y (assoc (get grid y) x ch)))

(defn add-player-to-grid
  [grid coords]
  (add-char-to-grid grid coords player-char))

(defn add-robot-to-grid
  [grid coords]
  (add-char-to-grid grid coords robot-char))

(defn add-pile-to-grid
  [grid coords]
  (add-char-to-grid grid coords pile-char))

(defn add-robots-to-grid
  [grid robots]
  (reduce add-robot-to-grid grid robots))

(defn add-piles-to-grid
  [grid piles]
  (reduce add-pile-to-grid grid piles))

(defn board->grid
  [board]
  (-> (empty-grid)
      (add-player-to-grid (:player board))
      (add-robots-to-grid (:robots board))
      (add-piles-to-grid (:piles board))))

(defn grid->vos
  "Return a vector of strings representing the grid"
  [grid]
  (vec (map #(apply str %) grid)))

(defn board->vos
  "Return a vector of strings representing the board"
  [board]
  (grid->vos (board->grid board)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
