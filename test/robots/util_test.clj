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

(deftest test-append-strings
  (testing "same length"
    (is (= ["aadd" "bbee" "ccff"] (append-strings ["aa" "bb" "cc"] ["dd" "ee" "ff"]))))
  (testing "first longer"
    (is (= ["aadd" "bbee" "cc"  ] (append-strings ["aa" "bb" "cc"] ["dd" "ee"     ]))))
  (testing "first shorter"
    (is (= ["aadd" "bbee" "ff"  ] (append-strings ["aa" "bb"     ] ["dd" "ee" "ff"])))))
