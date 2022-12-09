(ns aoc2022.core
  (:require [clojure.string :as string]))

;(def path "resources/day-9-test.txt")
;(def path "resources/day-9-test-2.txt")
(def path "resources/day-9.txt")

(def steps (string/split (slurp path) #"\n"))

(defn update-head
  [step H]
  (case step
    \U (update H 0 dec)
    \R (update H 1 inc)
    \D (update H 0 inc)
    \L (update H 1 dec)))

(defn update-tail
  [head tail]
  (let [d (mapv - head tail)]
    (if (some #(< 1 (abs %)) d)
      (vec (map + tail
                (map #(cond
                        (> % 0) 1
                        (< % 0) -1
                        (= % 0) 0) d )))
      tail)))

(defn exp-steps
  [steps]
  (map #(string/split % #" ") steps))

(defn exp-step
  [step]
  (let [[dir-string n-string] (string/split step #" ")
        dir (first (char-array dir-string))
        n (Integer/parseInt n-string)]
    (repeat n dir)))

(defn exp-steps
  [steps]
  (apply concat (map exp-step steps)))

(defn walk
  [steps]
  (loop [rem-steps steps
         H [0 0]
         T [0 0]
         visited (into #{[0 0]})]
    (if (empty? rem-steps)
      visited
      (let [updated-H (update-head (first rem-steps) H)
            updated-T (update-tail updated-H T)]
        (recur (rest rem-steps)
               updated-H
               updated-T
               (conj visited updated-T))))))

(println "Day 9a result: " (count (walk (exp-steps steps))))

(defn update-knots
  [step knots]
  (loop [updated-knots (vector (update-head step (first knots)))
         remaining-knots (rest knots)]
    (if (empty? remaining-knots)
      updated-knots
      (recur (conj updated-knots (update-tail (last updated-knots) (first remaining-knots)))
             (rest remaining-knots)))))
  

(defn walk-many
  [steps]
  (loop [rem-steps steps
         knots (repeat 10 [0 0])
         visited  #{[0 0]}]
    (if (empty? rem-steps)
      visited
      (let [updated-knots (update-knots (first rem-steps) knots)]
        (recur (rest rem-steps)
               updated-knots
               (conj visited (last updated-knots)))))))


(println "Day 9b result: " (count (walk-many (exp-steps steps))))
