(ns robots.core-test
  (:require [clojure.test :refer :all]
            [robots.core :refer :all]))

(deftest test-pad
  (testing "empty coll"
    (is (= [:p :p :p] (pad [] 3 :p))))
  (testing "full coll"
    (is (= [:x :x :x] (pad [:x :x :x] 3 :p))))
  (testing "partial coll"
    (is (= [:x :x :p] (pad [:x :x] 3 :p))))
  (testing "beyond full coll"
    ; truncates to specified size
    (is (= [:x :x :x] (pad [:x :x :x :x] 3 :p)))))

(deftest test-move-coord
  (testing "move north"
    (is (= [5 4] (move-coord [5 5] :n))))
  (testing "move south"
    (is (= [5 6] (move-coord [5 5] :s))))
  (testing "move east"
    (is (= [6 5] (move-coord [5 5] :e))))
  (testing "move west"
    (is (= [4 5] (move-coord [5 5] :w))))
  (testing "move northeast"
    (is (= [6 4] (move-coord [5 5] :ne))))
  (testing "move southeast"
    (is (= [6 6] (move-coord [5 5] :se))))
  (testing "move northwest"
    (is (= [4 4] (move-coord [5 5] :nw))))
  (testing "move southwest"
    (is (= [4 6] (move-coord [5 5] :sw)))))

(deftest test-move-towards
  (is (= [4 3] (move-towards [5 2] [3 4])))
  (is (= [4 2] (move-towards [5 2] [3 2])))
  (is (= [4 1] (move-towards [5 2] [3 0])))
  ;; Already at target...
  (is (= [5 2] (move-towards [5 2] [5 2]))))

(deftest test-coord-in-bounds?
  (is (= true (coord-in-bounds? [0 0])))
  (is (= true (coord-in-bounds? [58 21])))
  (is (= true (coord-in-bounds? [5 5])))
  (is (= false (coord-in-bounds? [-1 -1])))
  (is (= false (coord-in-bounds? [58 22])))
  (is (= false (coord-in-bounds? [59 21]))))

(def sample-board {:player [5 2]
                   :robots #{[0 0] [7 1] [2 2] [4 4] [5 4]}
                   :piles #{[58 21]}})
(def sample-vos ["+                                                          "
                 "       +                                                   "
                 "  +  @                                                     "
                 "                                                           "
                 "    ++                                                     "
                 "                                                           "
                 "                                                           "
                 "                                                           "
                 "                                                           "
                 "                                                           "
                 "                                                           "
                 "                                                           "
                 "                                                           "
                 "                                                           "
                 "                                                           "
                 "                                                           "
                 "                                                           "
                 "                                                           "
                 "                                                           "
                 "                                                           "
                 "                                                           "
                 "                                                          *"])
(def sample-grid (apply str sample-vos))

(deftest test-board->vos
  (is (= sample-vos (board->vos sample-board))))

(deftest test-grid->board
  (is (= sample-board (grid->board sample-grid))))

(deftest test-move-robots
  (let [expected {:player [5 2]
                  :robots #{[1 1] [3 2] [6 2]}
                  :piles #{[5 3] [58 21]}}]
    (is (= expected (move-robots sample-board)))))

(deftest test-move-player
  (testing :wait
    (is (= sample-board (move-player sample-board :wait))))
  (testing :teleport
    (is (let [new-board (move-player sample-board :teleport)
              new-player (:player new-board)]
          (and (coord-in-bounds? new-player) (not= new-player (:player sample-board))))))
  (testing :n
    (is (= (assoc sample-board :player [5 1]) (move-player sample-board :n))))
  (testing :s
    (is (= (assoc sample-board :player [5 3]) (move-player sample-board :s))))
  (testing :e
    (is (= (assoc sample-board :player [6 2]) (move-player sample-board :e))))
  (testing :w
    (is (= (assoc sample-board :player [4 2]) (move-player sample-board :w))))
  (testing "unrecognized action"
    (is (= sample-board (move-player sample-board :foo)))))

(deftest test-player-alive?
  (testing "Player is alive"
    (is (= true (player-alive? sample-board))))
  (testing "Player is dead on a pile"
    (is (= false (player-alive? (assoc sample-board :player [58 21])))))
  (testing "Player is dead on a robot"
    (is (= false (player-alive? (assoc sample-board :player [2 2]))))))

(def near-full-vos ["+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
                    "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
                    "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
                    "+++   +++++++++++++++++++++++++++++++++++++++++++++++++++  "
                    "+++ @ +++++++++++++++++++++++++++++++++++++++++++++++++++  "
                    "+++   +++++++++++++++++++++++++++++++++++++++++++++++++++  "
                    "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
                    "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
                    "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
                    "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
                    "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
                    "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
                    "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
                    "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
                    "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
                    "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
                    "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
                    "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
                    "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
                    "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
                    "  +++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
                    "  +++++++++++++++++++++++++++++++++++++++++++++++++++++++++"])
(def near-full-grid (apply str near-full-vos))
(def near-full-board (grid->board near-full-grid))

(deftest test-safe-coords
  (is (= #{[4 4] [58 4] [0 21]} (safe-coords near-full-board))))
