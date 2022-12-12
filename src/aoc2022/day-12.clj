(ns aoc2022.core
  (:require [clojure.string :refer [split]]))

;(def path "resources/day-12-test.txt");
(def path "resources/day-12.txt")

(def height-map (mapv vec (split (slurp path) #"\n")))

(def map-rows (count height-map))
(def map-columns (count (first height-map)))

(defn find-char
 [ch h-map]
  (for [r (range map-rows)
        c (range map-columns)
        :when (= ch (get-in h-map [r c]))]
  [r c]))

(def starting-point (first (find-char \S height-map)))
(def end-point (first (find-char \E height-map)))

(def h-map
  (-> height-map
      (assoc-in starting-point \a)
      (assoc-in end-point \z)))


(defn init-path-map
  [h-map]
  (vec (repeat map-rows (vec (repeat map-columns nil)))))

(defn on-map?
 [[r c]]
 (and (<= 0 r (dec map-rows))
      (<= 0 c (dec map-columns))))

(defn possible-path?
 "Not too steep"
 [p q h-map]
 (>= 1 (- (int (get-in h-map q)) (int (get-in h-map p)))))

(defn not-checked?
  [p path-map]
  (not (get-in path-map p)))

(defn neighbouring-candidates
  [from h-map path-map]
  (let [raw-candidates (map #(mapv + from %) [[-1 0] [0 1] [1 0] [0 -1]])]
    (->> raw-candidates
         (filterv on-map?)
         (filterv #(possible-path? from % h-map))
         (filterv #(not-checked? % path-map))
         (vec))))

(defn update-path-map
  [points step path-map]
  (loop [points points
         path-map path-map]
    (if (empty? points)
      path-map
      (recur (rest points)
             (assoc-in path-map (first points) step)))))

(defn shortest-path
  [start end h-map]
  (loop [path-map (assoc-in (init-path-map h-map) start 0)
         candidates (neighbouring-candidates start h-map path-map)
         steps 1]
    (if (or (> steps (* map-columns map-rows)) (some #(= % end) candidates))
      steps
      (recur (update-path-map candidates steps path-map)
             (vec (distinct (apply concat (mapv #(neighbouring-candidates % h-map path-map) candidates))))
             (inc steps)))))

(println "Day 12a result: " (shortest-path starting-point end-point h-map))

(def possible-path-lengths (for [r (range map-rows)
      c (range map-columns)
      :when (= \a (get-in h-map [r c]))]
      (shortest-path [r c] end-point h-map)))

(println "Day 12b result: " (apply min possible-path-lengths))
