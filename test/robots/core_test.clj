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

(def sample-board {:player [5 2]
                   :robots [[0 0] [7 1] [3 2]]
                   :piles [[6 4] [58 21]]})
(def sample-vos ["+                                                          "
                 "       +                                                   "
                 "   + @                                                     "
                 "                                                           "
                 "      *                                                    "
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
