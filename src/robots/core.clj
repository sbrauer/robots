(ns robots.core
  (:require clojure.set)
  (:import  [jline.console ConsoleReader]))

(def ^:const cols 59)
(def ^:const rows 22)

(def ^:const player-char \@)
(def ^:const dead-player-char \X)
(def ^:const robot-char \+)
(def ^:const pile-char \*)
(def ^:const empty-char \space)

(defn pad
  [coll n padding]
  (take n (concat coll (repeat padding))))

; Coords are 2-item vectors [x y] (where top-left is [0 0])
; A "board" is a map with the coords of the :player, :robots, and :piles
; (where :robots and :piles are sets).
; A "grid" is a vector of characters representing the printable board.

(defn dir->offset
  [dir]
  (dir {:n [0 -1] :s [0 1] :e [1 0] :w [-1 0] :ne [1 -1] :se [1 1] :nw [-1 -1] :sw [-1 1]}))

(defn move-coord
  [coord dir]
  (map + coord (dir->offset dir)))

(defn coord-in-bounds?
  "Return true if the given coord is within the bounds specified by cols and rows."
  [[x y]]
  (and (<= 0 x)
       (<= 0 y)
       (< x cols)
       (< y rows)))

(defn rand-coord
  []
  [(rand-int cols) (rand-int rows)])

(defn empty-grid
  []
  (vec (repeat (* rows cols) empty-char)))

(defn rand-grid
  [num-robots]
  (shuffle
    (pad
      (cons player-char (repeat num-robots robot-char))
      (* rows cols)
      empty-char)))

(defn coord->grid-idx
  [x y]
  (+ x (* y cols)))

(defn grid-idx->coord
  [idx]
  [(rem idx cols) (quot idx cols)])

(defn add-char-to-grid
  [grid [x y] ch]
  (assoc grid (coord->grid-idx x y) ch))

(defn add-player-to-grid
  [grid coords alive?]
  (add-char-to-grid grid coords (if alive? player-char dead-player-char)))

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

;; This implementation assumes that the grid represents a board with a single alive player.
(defn grid->board
  [grid]
  (let [char-map (group-by
                   last
                   (map-indexed
                     (fn [idx ch] [(grid-idx->coord idx) ch])
                     grid))]
    {:player (first (first (char-map player-char)))
     :robots (set (map first (char-map robot-char)))
     :piles (set (map first (char-map pile-char)))}))

(defn grid->vos
  "Return a vector of strings representing the grid"
  [grid]
  (vec (map #(apply str %) (partition cols grid))))

(defn border
  []
  (apply str (flatten [\+ (repeat cols \-) \+])))

(defn grid->str
  "Return a string representing the grid (suitable for printing)"
  [grid border?]
  (if border?
    (apply str (interpose "\n"
                          (flatten [(border)
                                    (map #(format "|%s|" %) (grid->vos grid))
                                    (border)])))
    (apply str (interpose "\n" (grid->vos grid)))))

(defn grid->str
  "Return a string representing the grid (suitable for printing)"
  [grid border?]
  (apply str (interpose "\n"
    (if border?
      (flatten [(border)
                (map #(format "|%s|" %) (grid->vos grid))
                (border)])
      (grid->vos grid)))))

(defn rand-board
  [num-robots]
  (grid->board (rand-grid num-robots)))

(defn player-alive?
  [board]
  (not (contains? (clojure.set/union (:piles board) (:robots board)) (:player board))))

(defn count-robots-alive
  [board]
  (count (:robots board)))

(defn robots-alive?
  [board]
  (pos? (count-robots-alive board)))

(defn board->grid
  [board]
  (-> (empty-grid)
      (add-robots-to-grid (:robots board))
      (add-piles-to-grid (:piles board))
      (add-player-to-grid (:player board) (player-alive? board))))

(defn board->str
  [board border?]
  (grid->str (board->grid board) border?))

(defn board->vos
  "Return a vector of strings representing the board"
  [board]
  (grid->vos (board->grid board)))

(defn move-towards
  "Given two coordinates, return a new coord that gets source one step closer to target.
  (There's no change if source is already equal to target.)"
  [target source]
  (vec (map + source (map compare target source))))

(defn move-robots
  [board]
  (let [robots-by-pileup?
          (->> (:robots board)
               (map (partial move-towards (:player board)))
               (frequencies)
               (map (fn [[coord cnt]] [coord (< 1 cnt)]))
               (group-by second)
               (map #(vector (first %) (map first (second %))))
               (into {}))]
    (assoc board
      :robots (clojure.set/difference
                (set (get robots-by-pileup? false)) (:piles board))
      :piles  (clojure.set/union
                (set (get robots-by-pileup? true))  (:piles board)))))

(defn teleport
  "Return a random coord (not equal to original coord)."
  [coord]
  (first (filter #(not= coord %) (repeatedly rand-coord))))

(defn move-player
  "Handle a player action (:wait, :teleport, or a direction keyword)
  and return a new board."
  [board action]
  (case action
        :wait board
        :teleport (assoc board :player (teleport (:player board)))
        (:n :s :e :w :ne :nw :se :sw)
          (let [new-player (move-coord (:player board) action)]
            (assert (coord-in-bounds? new-player))
            (assoc board :player new-player))
        ;; Default (unrecognized action): just return the same board
        board))

(defn level->robots
  [level]
  (* 10 level))

(defn get-key
  []
  (let [cr (ConsoleReader.)
            keyint (.readCharacter cr)]
    (char keyint)))

(defn clear-screen
  []
  (print "\u001b[2J")
  (print "\u001B[0;0f"))

(defn yes-or-no?
  []
  (loop []
    (case (get-key)
      \y true
      \n false
      (recur))))

(defn get-action
  "Wait for any key and return the corresponding action keyword (or nil if unrecognized key)."
  []
  (case (get-key)
    (\space \. \5) :wait
    (\t \0) :teleport
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
    nil))

(defn dir-action?
  "Returns true if the given action corresponds to a direction."
  [action]
  (not (not (dir->offset action))))

(defn til-truthy
  [f]
  (first (filter identity (repeatedly f))))

;; History undo/redo stuff...

; FIXME: after getting it working, consider DRYing up undo and redo.
(defn undo
  [state undos redos]
  (let [other (peek undos)]
    (if other
      {:state other :undos (pop undos) :redos (conj redos state)}
      {:state state :undos undos :redos redos})))

(defn redo
  [state undos redos]
  (let [other (peek redos)]
    (if other
      {:state other :redos (pop redos) :undos (conj undos state)}
      {:state state :undos undos :redos redos})))

(defn historize
  "Decorates the given function with undo/redo.
  f is a function that takes [state action]
  and returns a new state.
  Returns a function with the same arguments that intercepts
  the actions :undo and :redo and returns the corresponding state."
  [f]
  (let [history (atom {:undos []
                       :redos []})]
    (fn
      [orig-state action]
      (let [{:keys [state undos redos]}
            (case action
              :undo (undo orig-state (:undos @history) (:redos @history))
              :redo (redo orig-state (:undos @history) (:redos @history))
              (let [new-state (f orig-state action)]
                {:state new-state
                 :undos (conj (:undos @history) orig-state)
                 :redos []}))]
            (when (not= state orig-state)
              (swap! history #(assoc % :undos undos :redos redos)))
            state))))

(defn render-game
  [level board]
  (clear-screen)
  (println (str "Level: " level " - Robots: " (count-robots-alive board) " of " (level->robots level)))
  (println (board->str board true))
  (when-not (player-alive? board) (println "*** OH NO! KILLED BY A ROBOT! GAME OVER ***")))

(defn valid-action?
  [action board]
  (if action
    (if (dir-action? action)
      (coord-in-bounds? (move-coord (:player board) action))
      true)
    false))

(defn validate-action
  "Returns action if it's valid; else returns nil."
  [action board]
  (if (valid-action? action board)
    action
    nil))

(defn get-valid-action
  "Won't return until it can return a valid action."
  [board]
  (til-truthy #(validate-action (get-action) board)))

(defn play-turn
  [board action]
  (let [new-board (move-player board action)]
    (if (player-alive? new-board)
      (move-robots new-board)
      new-board)))

(defn play-level
  "Return true if player completes level, or false if player dies."
  [level]
  (let
    [board (rand-board (level->robots level))
     ;; Wrap play-turn with history support (undo and redo)
     play-turn (historize play-turn)]
    (render-game level board)
    (loop [board board]
      (let [new-board (play-turn board (get-valid-action board))]
        (render-game level new-board)
        (if (player-alive? new-board)
          (if (robots-alive? new-board)
            (recur new-board)
            true)
          false)))))

(defn play-game
  []
  (loop [level 1]
    (if (play-level level)
      (recur (inc level)))))

(defn -main
  "Would you like to play a game?"
  [& args]
  (loop []
    (play-game)
    (print "Play again [yn] ")
    (flush)
    (if (yes-or-no?) (recur)))
  (println "Goodbye!"))
