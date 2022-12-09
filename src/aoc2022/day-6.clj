(ns aoc2022.core
  (:require [clojure.test :refer [deftest testing is]]))

(def n 4)

(defn end-of-start-marker-pos
  [n message]
  (let [message-vector (vec message)]
    (loop [index 0]
      (let [candidate (subvec message-vector index (+ index n))]
        (if (= (count (set candidate)) n)
          (+ index n)
          (recur (inc index)))))))
         

(deftest d6a
  (testing "samples"
    (is (= 7 (end-of-start-marker-pos 4 "mjqjpqmgbljsphdztnvjfqwrcgsmlb")))
    (is (= 5 (end-of-start-marker-pos 4 "bvwbjplbgvbhsrlpgdmjqwftvncz")))
    (is (= 6 (end-of-start-marker-pos 4 "nppdvjthqldpwncqszvftbrmjlhg")))
    (is (= 10 (end-of-start-marker-pos 4 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")))
    (is (= 11 (end-of-start-marker-pos 4 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")))))
           
(end-of-start-marker-pos 4 (slurp "resources/day-6.txt"))


(deftest d6b
  (testing "samples"
    (is (= 19 (end-of-start-marker-pos 14 "mjqjpqmgbljsphdztnvjfqwrcgsmlb")))
    (is (= 23 (end-of-start-marker-pos 14 "bvwbjplbgvbhsrlpgdmjqwftvncz")))
    (is (= 23 (end-of-start-marker-pos 14 "nppdvjthqldpwncqszvftbrmjlhg")))
    (is (= 29 (end-of-start-marker-pos 14 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")))
    (is (= 26 (end-of-start-marker-pos 14 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")))))
           
(end-of-start-marker-pos 14 (slurp "resources/day-6.txt"))
