(ns aoc2022.day-4
  (:require [clojure.string :as string]))

;(def path )
(def path "resources/day-4-test.txt")

(defn split
  [delimiter s]
  (string/split s delimiter))

(defn interval-contains
  "returns true if b is fully contained by a"
  ([a b]
   (let [[a1 a2] a
         [b1 b2] b]
     (and
      (<= a1 b1)
      (<= b2 a2))))
  ([[a b]]
   (interval-contains a b)))

(defn string->interval
  [s]
  (mapv #(Integer/parseInt %) (clojure.string/split s #"-" )))

(defn eiter-interval-contains-the-other?
  [[a b]]
  (or (interval-contains a b)
      (interval-contains b a)))

(defn intervals-overlap?
  [[a b]]
  (let [[a1 a2] a
        [b1 b2] b]
    (and (<= a1 b2)
         (<= b1 a2))))

(defn num-pairs-contains-the-other
  [path]
  (reduce
   +
   (map  #(->> %
               (split #",")
               (map string->interval)
               eiter-interval-contains-the-other?
               ({false 0 true 1}))
         (->> path
              slurp
              (split #"\n")))))

(defn num-intervals-overlap
  [path]
  (reduce
   +
   (map  #(->> %
               (split #",")
               (map string->interval)
               intervals-overlap?
               ({false 0 true 1}))
         (->> path
              slurp
              (split #"\n")))))

(println "Day-4a result: " (num-pairs-contains-the-other "resources/day-4.txt"))
(println "Day-4a result: " (num-intervals-overlap "resources/day-4.txt"))
