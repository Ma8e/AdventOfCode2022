(ns aoc2022.core-test
  (:require [clojure.test :refer :all]
            [aoc2022.core :refer :all]
            [clojure.string :as string]))

                                        ;day 1

(def calorie-file-path "resources/calories_test.txt")

(def calorie_distribution_string (slurp calory_file_path))
  
(deftest day-1
  (testing "max calories carried by elf"
    (is (= 24000 (find-max-calories calorie-file-path) )))
  (testing "max calories carried by top 3 elves"
    (is (=  45000(find-top-3-calories calorie-file-path) )))
  (testing "top n"
    (is (= [4 5 7] (top-n 3 [5 3 4 7 1]) ))
    (is (= [5 5 7] (top-n 3 [5 1 4 7 5]) ))))
