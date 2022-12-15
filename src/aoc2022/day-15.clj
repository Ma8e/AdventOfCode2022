(ns aoc2022.core
  (:require [clojure.string :refer [split join]]))

;(def path "resources/day-15-test.txt")
(def path "resources/day-15.txt")

(defn parse-row
  [s]
  (let [[sx sy bx by] (map #(Long/parseLong %) (re-seq #"-?\d+" s))]
    [[sy sx] [by bx]])) ; I prefer row first 

(def sensor-beacons (map
                     parse-row
                     (-> path
                         slurp
                         (split #"\n"))))

(defn manhattan-distance
  [[r1 c1] [r2 c2]]
  (+ (abs (- r1 r2)) (abs (- c1 c2))))

(defn sensor
  [sensor-beacon]
  {:sensor-pos (first sensor-beacon)
   :reach (apply manhattan-distance sensor-beacon)})

(def sensors (map sensor sensor-beacons))

(defn sensor-coverage-at-row
  [row sensor]
  (let [[sr sc] (:sensor-pos sensor)
        reach (:reach sensor)
        reach-at-row (- reach (abs (- row sr)))]
    (if (< reach-at-row 0)
      nil
      [(- sc reach-at-row) (+ sc reach-at-row)])))

(defn overlap?
  [[s1 e1]  [s2 e2]]
  (and (<= s1 e2) (<= s2 e1)))

(defn border?
  [[s1 e1]  [s2 e2]]
  (or (= (inc e1) s2)
      (= (inc e2) s1)))
      
(defn merge-intervals
  "assumes they are overlapping"
  [[s1 e1]  [s2 e2]]
  [(min s1 s2) (max e1 e2)])

(defn insert-interval
  [i-coll i]
  ;(println i-coll i)
  (cond
    (empty? i-coll) [i]
    (or (overlap? i (first i-coll))
        (border? i (first i-coll))) (recur
                                 (subvec i-coll 1)
                                 (merge-intervals i (first i-coll)))
    :default (vec (cons (first i-coll) (insert-interval (subvec i-coll 1) i)))))

(defn interval-length
  [[s e]]
  (- e s))

(defn row-coverage
  [sensors row]
  (reduce
   insert-interval
   []
   (remove nil? (map #(sensor-coverage-at-row row %) sensors))))

(defn intersect-intervals
  [[s1 e1 :as i1] [s2 e2 :as i2]]
  (if (overlap? i1 i2)
    [(max s1 s2) (min e1 e2)]
    nil))
    
; answer to first part
(map interval-length (row-coverage sensors 4000000))

; answer to the second part. Took about 45 seconds to run on my apple M2

(defn day-15b
  []
  (for [r (range 0 20)
        :let [rc (row-coverage sensors r)]
        :when (> (count rc) 1)]
    (let [c (inc (apply min (map second rc)))]
      {:row r :column c :intervals rc :tuning-frequency (+ (* c 4000000) r)})))


                                        ; visualisation


(defn in-interval?
  [n [s e]]
  (<= s n e))

(defn in-intervals?
  [n i-coll]
  (some #(in-interval? n %) i-coll))

(defn visualize
  [sensor-beacons [ns ne]]
  (let [sensors (map first sensor-beacons)
        beacons (map second sensor-beacons)]
    (println sensors)
    (println beacons)
    (for [r (range ns (inc ne))]
       (join \space (concat (list (format "%3d" r )) (for [c (range ns (inc ne))]
        (let [coverage (row-coverage (map sensor sensor-beacons) r)]
          (cond
            (some #{[r c]} sensors) \S
            (some #{[r c]} beacons) \B
            (in-intervals? c coverage) \#
            :default \.))))))))
                
            


