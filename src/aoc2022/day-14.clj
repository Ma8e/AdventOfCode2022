(ns aoc2022.core
  (:require [clojure.string :refer [split]]))

(def path "resources/day-14.txt")
;(def path "resources/day-14-test.txt")

(def rock-paths-strings
  (-> (slurp path)
      (split #"\n")))

(defn string->coordinate
  [s]
  (->> (split s #",")
      (mapv #(Long/parseLong %))))

(defn string->rock-path
  [s]
  (->> (split s #" -> ")
       (mapv string->coordinate)))

(def rock-paths (mapv string->rock-path rock-paths-strings))


(def max-depth
  (->> rock-paths
       (apply concat)
       (map second)
       (apply max)))

(defn insert-rock-path
  [world path]
  (loop [path path
         world world]
    (if (not (second path))
      world
      (recur (rest path)
             (let [[c1 r1] (first path)
                   [c2 r2] (second path)
                   [c1 c2] (sort [c1 c2])
                   [r1 r2] (sort [r1 r2])]
               (reduce
                #(assoc-in %1 %2 \#)
                world
                (for [r (range r1 (inc r2))
                      c (range c1 (inc c2))]
                  [r c])))))))

(defn insert-rock-paths
  [paths world]
  (reduce insert-rock-path world paths))

(def world
  (->> (vec (repeat (+ 2  max-depth) (vec (repeat 1000 \.))))
       (insert-rock-paths rock-paths)))

(defn sand-fall
  [sand-pos world]
  (let [next-sand-pos
        (first
         (filter
          #(= \. (get-in world %))
          (map #(map + sand-pos %) [[1 0] [1 -1] [1 1]])))]
    (if (not next-sand-pos)
      (assoc-in world sand-pos \o)
      (recur next-sand-pos world))))

(defn print-world
  [world]
  (println)
  (doseq [l world] (println (subvec l 480 520))))

(defn drop-sand-1
  [world counter]
  (if (first (filter #(= \o %) (last world)))
    (do (print-world world)
        (dec counter))
    (recur (sand-fall [0 500] world) (inc counter))))

(defn drop-sand-2
  [world counter]
  (if (= (get-in world [0 500]) \o)
    (do (print-world world)
        counter)
    (recur (sand-fall [0 500] world) (inc counter))))
