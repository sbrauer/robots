(ns robots.board-test
  (:require [clojure.test :refer :all]
            [robots.board :refer :all]
            [robots.coord :as coord]
            [robots.fixtures :refer :all]))

(deftest test-board->strings
  (is (= sample-strings (board->strings sample-board))))

(deftest test-move-robots
  (let [expected {:player [5 2]
                  :robots #{[1 1] [3 2] [6 2]}
                  :piles #{[5 3] [58 21]}}]
    (is (= expected (move-robots sample-board)))))

(deftest test-move-player
  (testing :wait
    (is (= sample-board (move-player sample-board :wait))))
  (testing :teleport
    (let [new-board (move-player sample-board :teleport)
          new-player (:player new-board)]
      (is (and (coord/coord-in-bounds? new-player)
               (not= new-player (:player sample-board))))))
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

(deftest test-safe-coords
  (is (= #{[4 4] [58 4] [0 21]} (safe-coords near-full-board))))
