(ns robots.coord-test
  (:require [clojure.test :refer :all]
            [robots.coord :refer :all]))

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
