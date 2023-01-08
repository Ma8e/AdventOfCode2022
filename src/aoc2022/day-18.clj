(ns aoc2022.core
  (:require [clojure.string :refer [split]]))

(def cubes-test-string
  "2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5")

(defn parse-cube
  [row]
  (vec (map #(Long/parseLong %) (split row #","))))

(defn parse-cubes
  [s]
  (set (map parse-cube (split s #"\n"))))

(defn add-vec
  [v1 v2]
  (vec (map + v1 v2)))

(defn neighbours
  [cube]
  (let [pns '([1 0 0] [0 1 0] [0 0 1] [-1 0 0] [0 -1 0] [0 0 -1])]
    (set (map #(add-vec cube %) pns))))

(defn no-exposed-surfaces
  [cube cubes]
  (- 6 (count (keep (neighbours cube) cubes))))

(defn total-exposed-surfaces
  [cubes]
  (reduce + (map #(no-exposed-surfaces % cubes) cubes)))

;; test-data

(def cubes-test (parse-cubes cubes-test-string))
(total-exposed-surfaces cubes-test)

;; real data
(def cubes (parse-cubes (slurp "resources/day-18.txt")))
(total-exposed-surfaces cubes)

                                        ; part 2

(defn extremes
  [cubes]
  (letfn [(find-extr
    [fun]
    (vec 
     (for [d (range 3)]
       (apply fun (map #(nth % d) cubes)))))]
  {:min (find-extr min) :max (find-extr max)}))

(defn within?
  [cube {:keys [min max]}]
  #_(println cube min max)
  (every?
   #(<= (dec (nth min %)) (nth cube %) (inc (nth max %)))
   (range 3)))

(defn exterior
  [cubes]
  (let [extr (extremes cubes)]
    (loop [current #{(:min extr)}
           exterior #{}]
      #_(println current exterior)
      (if (empty? current)
        exterior
        (recur
         (->> current
              (map neighbours)
              (apply concat)
              (filter #(within? % extr))
              (remove cubes)
              (remove exterior)
              set)
         (set (concat exterior current)))))))

(defn no-externally-exposed-surfaces
  [cube cubes exterior]
  (count (keep (neighbours cube) exterior)))

(defn total-exposed-surfaces-2
  [cubes]
  (let [ext (exterior cubes)]
  (reduce + (map #(no-externally-exposed-surfaces % cubes ext) cubes))))
