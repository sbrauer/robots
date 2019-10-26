(ns robots.game
  (:require [robots.board     :as board]
            [robots.constants :as const]
            [robots.coord     :as coord]
            [robots.grid      :as grid]
            [robots.history   :as history]
            [robots.terminal  :as term]
            [robots.util      :as util]))

(defn level->robots
  [level]
  (* const/robots-per-level level))

(defn level->rand-board
  [level]
  (board/rand-board (level->robots level)))

(defn player-screen-coord
  "Returns coord of player on the screen (accounting for border)."
  [board]
  (map + (:player board) [2 2]))

(defn render-game
  [board level num-moves]
  (term/clear-screen)
  (let [board-strings (util/add-border-to-strings (board/board->strings board))
        alive? (board/player-alive? board)
        sidebar ["" (str " Level " level)
                 "" (str " Moves " num-moves)
                 "" (str " Robots " (board/count-robots board) "/" (level->robots level))
                 "" (str " Piles " (board/count-piles board))
                 "" (if alive? " Alive :)" " *** DEAD ***")]]
    (println (apply str (interpose "\n" (util/append-strings board-strings sidebar))))
    (when alive?
      (print "Move HJKLYUBN or numpad [T]teleport [space]wait [Z]undo [X]redo")
      (term/move-cursor (player-screen-coord board))))
    (flush))

(defn get-action
  "Wait for a key press that corresponds to an action and return the action keyword."
  []
  (case (term/get-key)
    (\space \. \5) :wait
    (\t \0) :teleport
    \s      :safe-teleport
    \w      :wait-for-end
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

(defn valid-action?
  "Is the given action valid (non-nil and resulting in a possible move)."
  [action board]
  (case action
    (:n :s :e :w :ne :nw :se :sw) (coord/coord-in-bounds? (coord/move-coord (:player board) action))
    (:wait :teleport :undo :redo :wait-for-end) true
    :safe-teleport (boolean (board/safe-teleport board))
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
  (let [new-board (board/move-player board action)]
    (if (board/player-alive? new-board)
      (board/move-robots new-board)
      new-board)))

(def play-turn-with-history (history/historize play-turn))

(defn play-board
  "Returns one of :random :retry :success :newgame :quit"
  [board level]
  (render-game board level 0)
  (loop [history-context {:state board}]
    (let [action (get-valid-action (:state history-context))
          new-context (play-turn-with-history history-context action)
          board (:state new-context)
          num-moves (count (:undos new-context))]
      (render-game board level num-moves)
      (if (board/player-alive? board)
        (if (board/robots-alive? board)
          (recur new-context)
          :success)
        (let [result (handle-death)]
          ;; Undoing death is a bit of special case...
          (if (= result :undo)
            (let [{:keys [state undos] :as new-context} (play-turn-with-history new-context :undo)]
              (render-game state level (count undos))
              (recur new-context))
            result))))))

(defn play-level
  "Returns one of :success :newgame :quit"
  [level]
  (loop [board (level->rand-board level)]
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
