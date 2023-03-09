(ns aoc2022.core
  (:require [clojure.string :refer [split]]
            [clojure.test :refer [deftest is run-tests]]))

(defn make-pairs
  [numbers]
  (mapv vector numbers (repeat false)))

(deftest make-pairs-test
  (is (= [[3 false] [5 false]] (make-pairs [3 5]))))

(defn unmake-pairs
  [pairs]
  (mapv first pairs))

(defn parse-input
  [path]
  (mapv
   #(Long/parseLong %)
   (-> path
       slurp
       (split #"\n"))))

(def numbers-test (parse-input "resources/day-20-test.txt"))
(def numbers (parse-input "resources/day-20.txt"))

(defn make-indexed-numbers
  [numbers]
  (mapv #(vector (* %1 811589153) %2) numbers (range)))

(def indexed-numbers-test (make-indexed-numbers numbers-test))
(def indexed-numbers (make-indexed-numbers numbers))

(defn first-unmoved
  [numbers]
  (first (keep-indexed #(when (not (second %2)) [%1 (first %2)]) numbers)))

(deftest first-unmoved-test
  (is (= (first-unmoved [[3 true]]) nil))
  (is (= (first-unmoved [[3 false] [4 false]]) [0 3]))
  (is (= (first-unmoved [[3 true] [4 false]]) [1 4]))
  (is (= (first-unmoved [[3 false] [4 true]]) [0 3])))
  
(defn move-number
  [[current-pos number] pairs]
  (let [new-pos (mod (+ current-pos number) (dec (count pairs)))]
    (vec 
     (if (< current-pos new-pos)
       (concat (subvec pairs 0 current-pos)
               (subvec pairs (inc current-pos) (inc new-pos))
               [[number true]]
               (subvec pairs (inc new-pos)))
       (concat (subvec pairs 0 new-pos)
               [[number true]]
               (subvec pairs new-pos current-pos)
               (subvec pairs (inc current-pos)))))))

(deftest move-number-test
  (is (= (move-number [1 -1] [[1 false] [-1 false] [3 false]]) [[-1 true] [1 false] [3 false]]))
  (is (= (move-number [1 0] [[1 false] [0 false] [3 false]]) [[1 false] [0 true] [3 false]])) 
  (is (= (move-number [1 1] [[0 false] [1 false] [9 false]]) [[1 true ] [0 false] [9 false]]))
  (is (= (move-number [1 2] [[0 false] [2 false] [9 false]]) [[0 false] [2 true] [9 false]]))
  (is (= (move-number [1 3] [[0 false] [3 false] [9 false]]) [[3 true] [0 false] [9 false]]))
  (is (= (move-number [1 4] [[0 false] [4 false] [9 false]]) [[0 false] [4 true] [9 false]]))
  (is (= (move-number [1 -1] [[0 false] [-1 false] [9 false]]) [[-1 true] [0 false] [9 false]]))
  (is (= (move-number [1 -2] [[0 false] [-2 false] [9 false]]) [[0 false] [-2 true] [9 false]]))
  (is (= (move-number [1 -3] [[0 false] [-3 false] [9 false]]) [[-3 true] [0 false] [9 false]]))
  (is (= (move-number [1 -4] [[0 false] [-4 false] [9 false]]) [[0 false] [-4 true] [9 false]])))

(defn move-number-2
  [current-pos indexed-number indexed-numbers]
    (let [new-pos (mod (+ current-pos (first indexed-number)) (dec (count indexed-numbers)))]
    (vec 
     (if (< current-pos new-pos)
       (concat (subvec indexed-numbers 0 current-pos)
               (subvec indexed-numbers (inc current-pos) (inc new-pos))
               [indexed-number]
               (subvec indexed-numbers (inc new-pos)))
       (concat (subvec indexed-numbers 0 new-pos)
               [indexed-number]
               (subvec indexed-numbers new-pos current-pos)
               (subvec indexed-numbers (inc current-pos)))))))

(defn mix
  [numbers]
  (loop [pairs (make-pairs numbers)]
    (if-let [[current-pos number] (first-unmoved pairs)]
      (recur (move-number [current-pos number] pairs))
      (unmake-pairs pairs))))

(deftest mix-test
  (is (= (mix numbers-test) [-2, 1, 2, -3, 4, 0, 3])))

(defn find-indexed
  [i indexed-numbers]
  (first (keep-indexed #(when (= i (second %2)) [%1 %2]) indexed-numbers)))

(deftest find-indexed-test
  (is (= (find-indexed 3 [[104 4] [103 3] [105 5]]) [1 [103 3]]))
  (is (= (find-indexed 7 [[104 4] [103 3] [105 5]]) nil)))

(defn mix-indexed-numbers
  [indexed-numbers]
  (let [c (count indexed-numbers)]
    (loop [indx 0
           indexed-numbers indexed-numbers]
      (if (>= indx c)
        indexed-numbers
        (let [[current-pos indexed-number] (find-indexed indx indexed-numbers)]
          (recur
           (inc indx)
           (move-number-2 current-pos indexed-number indexed-numbers)))))))



(defn grove-coordinates
  [mixed-numbers]
  (let [start-pos (first (keep-indexed #(when (= 0 %2) %1) mixed-numbers))
        c (count mixed-numbers)
        inds (map #(mod (+ start-pos %) c) '(1000 2000 3000))]
    (map #(nth mixed-numbers %) inds)))

(deftest grove-coordinates-test
  (is (= (grove-coordinates (mix numbers-test)) [4 -3 2])))

#_(def answer-part-1 (apply + (grove-coordinates (mix numbers))))
(def answer-part-2 (apply + (grove-coordinates (mapv first (nth (iterate mix-indexed-numbers indexed-numbers) 10)))))

(run-tests)
