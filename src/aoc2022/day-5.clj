(ns aoc2022.core
  (:require [clojure.string :as string]))

;(def path "resources/day-5-test.txt")
(def path "resources/day-5.txt")

(def data (string/split (slurp path) #"\n"))

(def move-strings (filterv #(string/starts-with? % "move") data))

(defn remove-trailing-space
  [s]
  (vec (take-while #(not= \space %) s)))
  
(def stacks
   (->> 
    (apply
     map
     vector
     (->> data
          (filterv #(not (string/starts-with? % "move")))
          (filterv #(not (empty? %)))
          butlast
          (map #(string/split % #""))
          ))
    (mapv reverse)
    (drop 1)
    (take-nth 4)
    (mapv #(mapv first %))
    (mapv remove-trailing-space)
    ))


(defn move-one-instruction
  [m stacks]
  (let [[_ n _ from _ to] (string/split m #" ")
        n-crates (Integer/parseInt n)
        from-stack (dec (Integer/parseInt from))
        to-stack (dec (Integer/parseInt to))]
    (loop [s stacks
           times n-crates]
      (if (< times 1)
        s
        (let [crate (peek (nth s from-stack))]
          (recur (-> s
                     (update from-stack pop)
                     (update to-stack #(conj % crate)))
                 (dec times)))))))

(defn move
  [moves stacks]
  (if (empty? moves)
    stacks
    (move
     (rest moves)
     (move-one-instruction
      (first moves)
      stacks))))

(println "Result day 5a: " (apply str (map peek (move move-strings stacks))))


(defn move-n-instruction
  [m stacks]
  (let [[_ n _ from _ to] (string/split m #" ")
        n-crates (Integer/parseInt n)
        from-stack (dec (Integer/parseInt from))
        to-stack (dec (Integer/parseInt to))]
    (-> stacks
        (update ,, from-stack #(drop-last n-crates %))
        (update ,, to-stack #(concat % (take-last n-crates (nth stacks from-stack)))))))


(defn move-2
  [moves stacks]
  (if (empty? moves)
    stacks
    (move-2
     (rest moves)
     (move-n-instruction
      (first moves)
      stacks))))

(println "Result day 5b: " (apply str (map last (move-2 move-strings stacks))))


