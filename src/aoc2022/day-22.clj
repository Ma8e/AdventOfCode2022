(ns aoc2022.core
  (:require [clojure.string :refer [split split-lines]]
            [clojure.test :refer [deftest is run-tests with-test]]))

(defn pad-map
  [wmap]
  (let [width (apply max (map count wmap))]
    (mapv #(subvec (into % (repeat width \space)) 0 width) wmap)))

(defn parse-path-token
  [s]
  (cond
    (re-matches #"\d+" s) (Long/parseLong s)
    (re-matches #"[RL]" s) (first s)))
    
(with-test
  (defn parse-path
    [s]
    (if (empty? s)
      []
      (mapv parse-path-token (re-seq #"\d+|[RL]" s))))
  (is (= (parse-path "") []))
  (is (= (parse-path "R") [\R]))
  (is (= (parse-path "10") [10]))
  (is (= (parse-path "10R5L5R10L4R5L5") [10 \R 5 \L 5 \R 10 \L 4 \R 5 \L 5])))

(defn parse-file
  [path]
  (let [v (-> path
              slurp
              split-lines)
        c (count v)]
    {:map (pad-map (mapv vec (subvec v 0 (- c 2))))
     :path (parse-path (last v))}))

(def notes-test (parse-file "resources/day-22-test.txt"))
(def map-test (:map notes-test))
(def path-test (:path notes-test))

(def notes (parse-file "resources/day-22.txt"))
(def wmap (:map notes))
(def path (:path notes))

(defn print-map
  [map]
  (doseq [row map]
    (doseq [c row]
      (print c))
    (println)))

(defn new-pos
  [row col dir]
  {:row row :col col :direction dir})

(defn step
  ([[row col] direction]
   (case direction
     :east [row (inc col)]
     :north [(dec row) col]
     :west [row (dec col)]
     :south [(inc row) col]))
  ([{:keys [row col direction]}]
   (let [[row col] (step [row col] direction)]
     (new-pos row col direction))))

(defn on-map
  [[row col] worldmap]
  (let [new-row (mod row (count worldmap))]
    [new-row
     (mod col (count (get worldmap new-row)))]))

(with-test
  (defn move
    [[row col] direction steps worldmap]
    (let [map-width (apply max (map count worldmap))
          map-height (count worldmap)]
      (loop [row row
             col col
             last-perm-row row
             last-perm-col col
             steps steps]
        (if (= steps 0)
          [row col]
          (let [[cand-row cand-col] (on-map (step [row col] direction) worldmap)]
;            (println cand-row map-height cand-col (get-in worldmap [cand-row cand-col]))
            (case (get-in worldmap [cand-row cand-col])
              \# [last-perm-row last-perm-col]
              \. (recur cand-row cand-col cand-row cand-col (dec steps))
              \space (recur cand-row cand-col last-perm-row last-perm-col steps)))))))
  (is (= (move [0 8] :east 1 map-test) [0 9]))
  (is (= (move [0 8] :east 2 map-test) [0 10]))
  (is (= (move [0 8] :east 3 map-test) [0 10])) ; blocked
  (is (= (move [0 8] :south 1 map-test) [1 8]))
  (is (= (move [0 8] :south 2 map-test) [1 8])) ; blocked
  (is (= (move [3 8] :east 3 map-test) [3 11]))
  (is (= (move [0 3] :east 1 [(vec (repeat 4 \.))]) [0 0]))
  (is (= (move [3 0] :south 1 (vec (repeat 5 [\. \.]))) [4 0]))
  (is (= (move [3 0] :south 2 (vec (repeat 5 [\. \.]))) [0 0]))
  (is (= (move [0 2] :east 2 [[\. \. \. \space \space]]) [0 1]))
  (is (= (move [0 2] :east 2 [[\space \. \. \.]]) [0 1]))
  (is (= (move [0 2] :east 3 [[\space \space \. \. \.]]) [0 2]))
  (is (= (move [0 0] :west 1  [[\. \. \.]]) [0 2]))
  (is (= (move [0 0] :north 2  (vec (repeat 3 [\. \.]))) [1 0]))
  (is (= (move [1 2] :south 2  [[\. \. \.][\. \. \.][\. \. \space][\. \. \space]]) [1 2]))
  (is (= (move [2 0] :south 2  [[\space \. \.][\# \. \.][\. \. \.][\space \. \.]]) [2 0])))

(defn new-dir
  [old-dir t]
  (if (= t \L)
    (case old-dir
      :east :north
      :north :west
      :west :south
      :south :east)
    (case old-dir
      :east :south
      :south :west
      :west :north
      :north :east)))

(defn turn
  [t pos]
  (update pos :direction new-dir t))

(defn init-pos
  [wmap]
  {:row 0 :col (first (keep-indexed #(if (= %2 \.) %1) (first wmap))) :direction :east})

(defn move-pos
  [pos steps wmap]
  (let [[row col] (move [(:row pos) (:col pos)] (:direction pos) steps wmap)]
    {:row row :col col :direction (:direction pos)}))

(defn follow-path
  [pos path wmap]
  #_(println pos (first path))
  (cond
    (empty? path) pos
    (number? (first path)) (recur
                            (move-pos pos (first path) wmap)
                            (rest path)
                            wmap)
    :else (recur
           (turn (first path) pos)
           (rest path)
           wmap)))

(defn password
  [pos]
  (+ 
   (* 1000 (inc (:row pos)))
   (* 4 (inc (:col pos)))
   (case (:direction pos)
     :east 0
     :south 1
     :west 2
     :north 3)))

(run-tests)

(def day-22-part-1-answer (password (follow-path (init-pos wmap) path wmap)))

day-22-part-1-answer

