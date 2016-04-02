(ns robots.board
  (:require clojure.set
            [robots.coord :as coord]
            [robots.grid  :as grid]))

; A "board" is a map with the coords of the :player, :robots, and :piles
; (where :robots and :piles are sets).

(defn player-alive?
  [board]
  (not (contains? (clojure.set/union (:piles board) (:robots board)) (:player board))))

(defn count-piles
  [board]
  (count (:piles board)))

(defn count-robots-alive
  [board]
  (count (:robots board)))

(defn robots-alive?
  [board]
  (pos? (count-robots-alive board)))

(defn board->grid
  [board]
  (-> (grid/empty-grid)
      (grid/add-robots-to-grid (:robots board))
      (grid/add-piles-to-grid (:piles board))
      (grid/add-player-to-grid (:player board) (player-alive? board))))

(defn board->str
  [board]
  (grid/grid->str (board->grid board)))

(defn board->vos
  "Return a vector of strings representing the board"
  [board]
  (grid/grid->vos (board->grid board)))

(defn safe-coord?
  [board co]
  (not (or (contains? (:piles board) co)
           (contains? (:robots board) co)
           (contains? (:robots board) (coord/move-coord co :n))
           (contains? (:robots board) (coord/move-coord co :s))
           (contains? (:robots board) (coord/move-coord co :e))
           (contains? (:robots board) (coord/move-coord co :w))
           (contains? (:robots board) (coord/move-coord co :ne))
           (contains? (:robots board) (coord/move-coord co :nw))
           (contains? (:robots board) (coord/move-coord co :se))
           (contains? (:robots board) (coord/move-coord co :sw)))))

(defn safe-coords
  [board]
  (set (filter (partial safe-coord? board) (coord/all-board-coords))))

(defn teleport
  "Return a random coord (not equal to original coord)."
  [board]
  (first (filter #(not= (:player board) %) (repeatedly coord/rand-coord))))

(defn safe-teleport
  "Return a random coord (not equal to player or a robot or pile) or nil if impossible."
  [board]
  (let [safe-options (disj (safe-coords board) (:player board))]
    (if (empty? safe-options)
      nil
      (rand-nth (vec safe-options)))))

(defn move-player
  "Handle a player action (:wait, :teleport, :safe-teleport or a direction keyword)
  and return a new board."
  [board action]
  (case action
        :teleport (assoc board :player (teleport board))
        :safe-teleport (assoc board :player (safe-teleport board))
        (:n :s :e :w :ne :nw :se :sw)
          (let [new-player (coord/move-coord (:player board) action)]
            (assert (coord/coord-in-bounds? new-player))
            (assoc board :player new-player))
        ;; Default (:wait or unrecognized action) just return the same board.
        board))

(defn move-robots
  [board]
  (let [robots-by-pileup?
          (->> (:robots board)
               (map (partial coord/move-towards (:player board)))
               (frequencies)
               (map (fn [[co cnt]] [co (< 1 cnt)]))
               (group-by second)
               (map #(vector (first %) (map first (second %))))
               (into {}))]
    (assoc board
      :robots (clojure.set/difference
                (set (get robots-by-pileup? false)) (:piles board))
      :piles  (clojure.set/union
                (set (get robots-by-pileup? true))  (:piles board)))))
