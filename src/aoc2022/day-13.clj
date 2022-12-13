(ns aoc2022.core
  (:require [clojure.string :refer [split replace]]))

;(def path "resources/day-13-test.txt")
(def path "resources/day-13.txt")

(defn read-pair
  [p]
  (mapv read-string p))

(defn read-pair-list
  [l]
  (mapv read-pair l))

(def pairs
  (->>
   (slurp path)
   (#(split % #"\n\n"))
   (mapv #(split % #"\n"))
   read-pair-list))

(ns-unmap *ns* 'compare-vectors)
(ns-unmap *ns* 'mcomp)

(declare mcomp)

(defmulti compare-vectors (fn [[l r]] [(empty? l) (empty? r)]))
(defmethod compare-vectors [true true] [_] 0)
(defmethod compare-vectors [true false] [_] -1)
(defmethod compare-vectors [false true] [_] 1)
(defmethod compare-vectors [false false] [[l r]]
  (let [c (mcomp [(first l) (first r)])]
    (if (= c 0)
      (compare-vectors [(subvec l 1) (subvec r 1)])
      c)))

(defmulti mcomp (fn [[l r]] [(class l) (class r)]))
(defmethod mcomp [java.lang.Long java.lang.Long] [[l r]] (compare l r))
(defmethod mcomp [clojure.lang.PersistentVector clojure.lang.PersistentVector] [[l r]]
  (compare-vectors [l r]))
(defmethod mcomp [java.lang.Long clojure.lang.PersistentVector] [[l r]]
  (mcomp [[l] r]))
(defmethod mcomp [clojure.lang.PersistentVector java.lang.Long] [[l r]]
  (mcomp [l [r]]))
(defmethod mcomp [clojure.lang.PersistentVector clojure.lang.PersistentVector] [[l r]]
  (compare-vectors [l r]))

(println
 "Day 13a result: "
 (reduce
  +
  (mapv
   #(* (inc %1) %2)
   (range)
   (mapv
    #({-1 1 1 0} (mcomp %))
    pairs))))

(def divider-packets [[[2]] [[6]]])

(def items
  (-> (slurp path)
      (replace "\n\n" "\n")
      (split #"\n")
      (#(mapv read-string %))
      (concat divider-packets)))

(defn mcomparator [l r]
  (mcomp [l r]))

(println "Day 13b results: "
         (reduce * (map #(inc (.indexOf (sort mcomparator items) %)) divider-packets)))
