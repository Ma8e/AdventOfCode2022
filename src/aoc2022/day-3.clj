(ns aoc2022.day-3
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(def rucksacks-string (slurp "resources/day-3.txt"))

(defn string->rucksack
  [s]
  (split-at (/ (count s) 2) s))

(defn shared-item
  [[first-comp second-comp]]
  (first (set/intersection (set first-comp) (set second-comp))))

(defn priority
  [c]
  (if (Character/isUpperCase c)
    (+ (- (int c) (int \A)) 27)
    (+ (- (int c) (int \a)) 1)))

(defn string->rucksacks
  [s]
  (map string->rucksack (string/split s #"\n")))

(defn priority-sum
  [path]
  (reduce
   +
   (->> path
        slurp
        string->rucksacks
        (map shared-item)
        (map priority)
        )))
      
(println "Day 3a result: " (priority-sum "resources/day-3.txt"))

(defn group-badge-priority-sum
  [path]
  (reduce
   +
   (->> path
        slurp
        (#(string/split % #"\n"))
        (map set)
        (partition 3)
        (map #(apply set/intersection %))
        (map first)
        (map priority))))

(println "Day 3b result: " (group-badge-priority-sum "resources/day-3.txt"))
