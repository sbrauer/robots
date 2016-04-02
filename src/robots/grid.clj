(ns robots.grid
  (:require [robots.constants :as const]
            [robots.util :as util]))

; A "grid" is a vector of characters representing the printable board.

(defn empty-grid
  []
  (vec (repeat (* const/rows const/cols) const/empty-char)))

(defn rand-grid
  [num-robots]
  (shuffle
    (util/pad
      (cons const/player-char (repeat num-robots const/robot-char))
      (* const/rows const/cols)
      const/empty-char)))

(defn coord->grid-idx
  [[x y]]
  (+ x (* y const/cols)))

(defn grid-idx->coord
  [idx]
  [(rem idx const/cols) (quot idx const/cols)])

(defn add-char-to-grid
  [grid coord ch]
  (assoc grid (coord->grid-idx coord) ch))

(defn add-player-to-grid
  [grid coords alive?]
  (add-char-to-grid grid coords (if alive? const/player-char const/dead-player-char)))

(defn add-robot-to-grid
  [grid coords]
  (add-char-to-grid grid coords const/robot-char))

(defn add-pile-to-grid
  [grid coords]
  (add-char-to-grid grid coords const/pile-char))

(defn add-robots-to-grid
  [grid robots]
  (reduce add-robot-to-grid grid robots))

(defn add-piles-to-grid
  [grid piles]
  (reduce add-pile-to-grid grid piles))

;; This implementation assumes that the grid represents a board with a single alive player.
(defn grid->board
  [grid]
  (let [char-map (group-by
                   last
                   (map-indexed
                     (fn [idx ch] [(grid-idx->coord idx) ch])
                     grid))]
    {:player (first (first (char-map const/player-char)))
     :robots (set (map first (char-map const/robot-char)))
     :piles (set (map first (char-map const/pile-char)))}))

(defn grid->vos
  "Return a vector of strings representing the grid"
  [grid]
  (mapv #(apply str %) (partition const/cols grid)))

(defn grid->str
  "Return a string representing the grid (suitable for printing)"
  [grid]
  (apply str (interpose "\n" (grid->vos grid))))
