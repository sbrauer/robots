(ns robots.grid-test
  (:require [clojure.test :refer :all]
            [robots.grid :refer :all]
            [robots.fixtures :refer :all]))

(deftest test-grid->board
  (is (= sample-board (grid->board sample-grid))))
