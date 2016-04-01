(ns robots.core
  (:gen-class)
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
  [[x y]]
  (+ x (* y cols)))

(defn grid-idx->coord
  [idx]
  [(rem idx cols) (quot idx cols)])

(defn add-char-to-grid
  [grid coord ch]
  (assoc grid (coord->grid-idx coord) ch))

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
  (mapv #(apply str %) (partition cols grid)))

(defn add-border-to-vos
  [vos]
  (let [width (count (first vos))
        border (apply str (flatten [\+ (repeat width \-) \+]))]
    (vec (concat [border] (map #(format "|%s|" %) vos) [border]))))

(defn grid->str
  "Return a string representing the grid (suitable for printing)"
  [grid]
  (apply str (interpose "\n" (grid->vos grid))))

(defn rand-board
  [num-robots]
  (grid->board (rand-grid num-robots)))

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
  (-> (empty-grid)
      (add-robots-to-grid (:robots board))
      (add-piles-to-grid (:piles board))
      (add-player-to-grid (:player board) (player-alive? board))))

(defn board->str
  [board]
  (grid->str (board->grid board)))

(defn board->vos
  "Return a vector of strings representing the board"
  [board]
  (grid->vos (board->grid board)))

(defn move-towards
  "Given two coordinates, return a new coord that gets source one step closer to target.
  (There's no change if source is already equal to target.)"
  [target source]
  (mapv + source (map compare target source)))

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

(defn safe-coord
  [board coord]
  (not (or (contains? (:piles board) coord)
           (contains? (:robots board) coord)
           (contains? (:robots board) (move-coord coord :n))
           (contains? (:robots board) (move-coord coord :s))
           (contains? (:robots board) (move-coord coord :e))
           (contains? (:robots board) (move-coord coord :w))
           (contains? (:robots board) (move-coord coord :ne))
           (contains? (:robots board) (move-coord coord :nw))
           (contains? (:robots board) (move-coord coord :se))
           (contains? (:robots board) (move-coord coord :sw)))))

(defn all-board-coords []
  (for [x (range cols)
        y (range rows)]
    [x y]))

(defn safe-coords
  [board]
  (set (filter (partial safe-coord board) (all-board-coords))))

(defn teleport
  "Return a random coord (not equal to original coord)."
  [board]
  (first (filter #(not= (:player board) %) (repeatedly rand-coord))))

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
          (let [new-player (move-coord (:player board) action)]
            (assert (coord-in-bounds? new-player))
            (assoc board :player new-player))
        ;; Default (:wait or unrecognized action) just return the same board.
        board))

(defn level->robots
  [level]
  (* 10 level))

(defn level->rand-board
  [level]
  (rand-board (level->robots level)))

(defn move-cursor
  [[x y]]
  (print (str "\u001b[" y ";" x "f")))

(defn clear-screen
  []
  (print "\u001b[2J")
  (move-cursor [0 0]))

(defn ring-bell
  []
  (print (char 7))
  (flush))

(defn get-key
  []
  (let [cr (ConsoleReader.)
            keyint (.readCharacter cr)]
    (char keyint)))

(defn get-action
  "Wait for a key press that corresponds to an action and return the action keyword."
  []
  (case (get-key)
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

(defn til-truthy
  [f]
  (first (filter identity (repeatedly f))))

;; History undo/redo stuff...

(defn undo
  [current-state undos redos]
  (let [old-state (peek undos)]
    (if old-state
      {:state old-state :undos (pop undos) :redos (conj redos current-state)}
      {:state current-state :undos undos :redos redos})))

(defn redo
  [current-state undos redos]
  (let [old-state (peek redos)]
    (if old-state
      {:state old-state :redos (pop redos) :undos (conj undos current-state)}
      {:state current-state :undos undos :redos redos})))

(defn historize
  "Decorates the given function with undo/redo.
  f is a function that takes [state action]
  and returns a new state.
  Returns a function with the same arguments that intercepts
  the actions :undo and :redo and returns the corresponding state.
  Note that instead of returning the new state itself, the decorated
  function returns a hash with the keys :state, :undos and :redos"
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
            (assoc @history :state state)))))

(defn player-screen-coord
  "Returns coord of player on the screen (accounting for border)."
  [board]
  (map + (:player board) [2 2]))

(defn append-to-vos
  "vos is a vector of strings.
  appends is a seq of pairs where first item is an index int and second is a string to append at that index.
  Returns a new vector of strings with the appends applied."
  [vos appends]
  (reduce
    (fn
      [coll [idx s]]
      (assoc coll idx (str (nth coll idx) s)))
    vos
    appends))

(defn render-game
  [board level moves]
  (clear-screen)
  (let [board-vos (add-border-to-vos (board->vos board))
        alive? (player-alive? board)
        appends [[1 (str " Level " level)]
                 [3 (str " Moves " moves)]
                 [5 (str " Robots " (count-robots-alive board) "/" (level->robots level))]
                 [7 (str " Piles " (count-piles board))]
                 [9 (if alive? " Alive :)" " *** DEAD ***")]]]
    (println (apply str (interpose "\n" (append-to-vos board-vos appends))))
    (when alive?
      (print "Move HJKLYUBN or numpad [T]teleport [space]wait [Z]undo [X]redo")
      (move-cursor (player-screen-coord board))))
    (flush))

(defn valid-action?
  "Is the given action valid (non-nil and resulting in a possible move)."
  [action board]
  (if action
    (case action
      :safe-teleport (boolean (safe-teleport board))
      (:n :s :e :w :ne :nw :se :sw) (coord-in-bounds? (move-coord (:player board) action))
      true)
    false))

(defn validate-action
  "Returns action if it's valid; else returns nil."
  [action board]
  (if (valid-action? action board)
    action
    ;; Ringing the bell as a side-effect isn't pure, but this is just a game.
    (ring-bell)))

(defn get-valid-action
  "Won't return until it can return a valid action."
  [board]
  (til-truthy #(validate-action (get-action) board)))

(defn get-post-death-action
  []
  (case (get-key)
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
  (let [play-turn-h (historize play-turn)]
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

(defn parse-int
  [s]
  (try
    (Integer/parseInt s)
    (catch NumberFormatException e
      nil)))

(defn -main
  "Would you like to play a game?"
  [& args]
  (let [level (or (parse-int (first args)) 1)]
    (loop []
      (if (play-game level) (recur))))
  (println "\nGoodbye!"))
