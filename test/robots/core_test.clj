(ns robots.core-test
  (:require [clojure.test :refer :all]
            [robots.core :refer :all]))

(deftest a-test
  (testing "board->vos"
    (let [board {:player [5 2]
                 :robots [[0 0] [7 1] [3 2]]
                 :piles [[6 4] [58 21]]}
          expected ["+                                                          "
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
                    "                                                          *"]]
      (is (= expected (board->vos board))))))
