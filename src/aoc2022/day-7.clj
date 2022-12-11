(ns aoc2022.core
  (:require [clojure.string :refer [split]]))

;(def path "resources/day-7-test.txt")
(def path "resources/day-7.txt")

(def input (mapv #(split % #" ") (split (slurp path) #"\n")))

(defn process-ls
  [input current-path fs]
  (loop [input input
         fs-part (get-in fs current-path)]
    (if (or (empty? input) (= (first (first input)) "$"))
      [(assoc-in fs current-path fs-part) input]
      (let [[a b] (first input)]
        (recur
         (rest input)
         (if (= a "dir")
           (if (contains? fs-part (keyword b))
             fs-part
             (assoc fs-part (keyword b) {} ))
           (assoc fs-part b (Integer/parseInt a))))))))          
  
(defn process-input
  [input]
  (loop [remaining-input input
         fs {}
         current-path []]
    (if (empty? remaining-input)
      fs
      (let [[_prompt command arg] (first remaining-input)]
        (case command
          "cd" (recur (rest remaining-input)
                      fs
                      (if (= arg "..")
                        (pop current-path)
                        (conj current-path (keyword arg))))
          "ls" (let [[fs remaining-input] (process-ls (rest remaining-input) current-path fs)]
                 (recur remaining-input
                        fs
                        current-path)))))))
          
    
(def fs (process-input input))

(defn item-size
  [v]
  (if (number? v)
    v
    (dir-size v)))

(defn dir-size
  [dir]
  (reduce + (map item-size (vals dir))))

(defn dir-size-list
  [dir]
  (flatten (conj (map dir-size-list (filter map? (vals dir))) (dir-size dir))))

(println "Day 7a result: " (reduce + (filter #(<= % 100000) (dir-size-list fs))))

(def total-disk-space 70000000)

(def free-space-needed 30000000)

(def disk-space-used (dir-size fs))

(def min-disk-space-to-free (- free-space-needed (- total-disk-space disk-space-used)))

(def actual-disk-space-to-be-freed (apply min (filter #(> % min-disk-space-to-free) (dir-size-list fs))))

(println "Day 7b result: " actual-disk-space-to-be-freed)
