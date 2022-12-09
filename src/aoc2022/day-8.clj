(ns aoc2022.day-8
  (:require [clojure.string :as string]))

;(def path "resources/day-8-test.txt")
(def path "resources/day-8.txt")

(def tree-string (slurp path))

(defn string-vec->num-vec
  [v]
  (mapv #(Integer/parseInt %) v))

(def tree (mapv string-vec->num-vec (mapv #(string/split % #"") (string/split tree-string #"\n"))))

tree

(comment 
[[3 0 3 7 3]
 [2 5 5 1 2]
 [6 5 3 3 2]
 [3 3 5 4 9]
 [3 5 3 9 0]])

(def width (count (first tree)))
(def height (count tree))

(def res (reduce
 +
 (for [r (range height)
       c (range width)]
   (if (or (= r 0) (= c 0) (= r (dec height)) (= c (dec width))
           (let [current-height (get-in tree [r c])
                 row (nth tree r)
                 column (map #(nth % c) tree)]
             (or (< (apply max (take c row)) current-height)
                 (< (apply max (take-last (- width c 1) row)) current-height)
                 (< (apply max (take r column)) current-height)
                 (< (apply max (take-last (- height r 1) column)) current-height))))
           1
           0))))

(println "Day 8a result: " res)

(defn view-to-the-right
  [from row]
  (let [width (count row)
        current-height (nth row from)]
    (loop [pos (inc from)
           res 0]
      (if (>= pos width)
        res
        (if (>= (nth row pos) current-height)
          (inc res)
          (recur (inc pos)
                 (inc res)))))))

(def max-view
  (apply
   max
   (flatten 
    (for [c (range width)]
      (let [column (map #(nth % c) tree)]
        (for [r (range height)]
          (let [row (nth tree r)]
            (* (view-to-the-right c row)
               (view-to-the-right (- width c 1) (reverse row))
               (view-to-the-right r column)
               (view-to-the-right (- height r 1) (reverse column))))))))))

(println "Day 8b result: " max-view)
