(ns aoc2022.day-3-test
  (:require [clojure.test :refer :all]
            [aoc2022.day-3 :refer :all]))


(deftest day-3
  (testing "day 3"
    (is (= 157 (priority-sum "resources/day-3-test.txt")))))
