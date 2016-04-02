(ns robots.core
  (:gen-class)
  (:require clojure.set
            [robots.constants :as const]
            [robots.coord     :as coord]
            [robots.grid      :as grid]
            [robots.history   :as history]
            [robots.term      :as term]
            [robots.util      :as util]))

; Coords are 2-item vectors [x y] (where top-left is [0 0])
; A "board" is a map with the coords of the :player, :robots, and :piles
; (where :robots and :piles are sets).
; A "grid" is a vector of characters representing the printable board.

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

(defn safe-coord
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
  (set (filter (partial safe-coord board) (coord/all-board-coords))))

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

(defn level->robots
  [level]
  (* 10 level))

(defn level->rand-board
  [level]
  (grid/rand-board (level->robots level)))

(defn get-action
  "Wait for a key press that corresponds to an action and return the action keyword."
  []
  (case (term/get-key)
    (\space \. \5) :wait
    (\t \0) :teleport
    \s      :safe-teleport
    (\k \8) :n
    (\j \2) :s
    (\h \4) :w
    (\l \6) :e
    (\u \9) :ne
    (\n \3) :se
    (\y \7) :nw
    (\b \1) :sw
    \z      :undo
    \x      :redo
    (recur)))

(defn player-screen-coord
  "Returns coord of player on the screen (accounting for border)."
  [board]
  (map + (:player board) [2 2]))

(defn render-game
  [board level moves]
  (term/clear-screen)
  (let [board-vos (util/add-border-to-vos (board->vos board))
        alive? (player-alive? board)
        appends [[1 (str " Level " level)]
                 [3 (str " Moves " moves)]
                 [5 (str " Robots " (count-robots-alive board) "/" (level->robots level))]
                 [7 (str " Piles " (count-piles board))]
                 [9 (if alive? " Alive :)" " *** DEAD ***")]]]
    (println (apply str (interpose "\n" (util/append-to-vos board-vos appends))))
    (when alive?
      (print "Move HJKLYUBN or numpad [T]teleport [space]wait [Z]undo [X]redo")
      (term/move-cursor (player-screen-coord board))))
    (flush))

(defn valid-action?
  "Is the given action valid (non-nil and resulting in a possible move)."
  [action board]
  (case action
    (:n :s :e :w :ne :nw :se :sw) (coord/coord-in-bounds? (coord/move-coord (:player board) action))
    (:wait :teleport :undo :redo) true
    :safe-teleport (boolean (safe-teleport board))
    false))

(defn validate-action
  "Returns action if it's valid; else returns nil."
  [action board]
  (if (valid-action? action board)
    action
    ;; Ringing the bell as a side-effect isn't pure, but this is just a game.
    (term/ring-bell)))

(defn get-valid-action
  "Won't return until it can return a valid action."
  [board]
  (util/til-truthy #(validate-action (get-action) board)))

(defn get-post-death-action
  []
  (case (term/get-key)
    \z :undo
    \t :retry
    \r :random
    \n :newgame
    \q :quit
    (recur)))

(defn handle-death
  "Prompt user for next action.
  Returns one of :undo :random :retry :newgame :quit"
  []
  (print "[Z]Undo, [T]ry again, [R]andom board, [N]ew game, [Q]uit? ")
  (flush)
  (get-post-death-action))

(defn play-turn
  [board action]
  (let [new-board (move-player board action)]
    (if (player-alive? new-board)
      (move-robots new-board)
      new-board)))

(defn play-board
  "Returns one of :random :retry :success :newgame :quit"
  [board level]
  (render-game board level 0)
  ;; Wrap play-turn with history support (undo and redo)
  (let [play-turn-h (history/historize play-turn)]
    (loop [board board]
      (let [{new-board :state undos :undos} (play-turn-h board (get-valid-action board))]
        (render-game new-board level (count undos))
        (if (player-alive? new-board)
          (if (robots-alive? new-board)
            (recur new-board)
            :success)
          (let [result (handle-death)]
            (if (= result :undo)
              (let [{new-board :state undos :undos} (play-turn-h new-board :undo)]
                (render-game new-board level (count undos))
                (recur new-board))
              result)))))))

(defn play-level
  "Returns one of :success :newgame :quit"
  [level]
  (loop
    [board (level->rand-board level)]
    (let [result (play-board board level)]
      (case result
        :retry (recur board)
        :random (recur (level->rand-board level))
        ;; presumably result is :success :newgame or :quit
        result))))

(defn play-game
  "Return true until player quits."
  [start-level]
  (loop [level start-level]
    (case (play-level level)
      :success (recur (inc level))
      :newgame true
      ;; presumably :quit
      false)))

(defn -main
  "Would you like to play a game?"
  [& args]
  (let [level (or (util/parse-int (first args)) 1)]
    (loop []
      (if (play-game level) (recur))))
  (println "\nGoodbye!"))
