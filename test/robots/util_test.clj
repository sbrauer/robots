(ns robots.util-test
  (:require [clojure.test :refer :all]
            [robots.util :refer :all]))

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

(deftest test-vos+vos
  (testing "same length"
    (is (= ["ad" "be" "cf"] (vos+vos ["a" "b" "c"] ["d" "e" "f"]))))
  (testing "first longer"
    (is (= ["ad" "be" "c"]  (vos+vos ["a" "b" "c"] ["d" "e"    ]))))
  (testing "first shorter"
    (is (= ["ad" "be" "f"]  (vos+vos ["a" "b"    ] ["d" "e" "f"])))))
