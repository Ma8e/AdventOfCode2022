(ns aoc2022.core
  (:require [clojure.string :as string])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

                                        ; day 1

(def day-1-data-path "resources/day-1.txt")

(defn parseLong [s] (Long/parseLong s)) ; because parseLong isn't static?

(defn string->calorie-distribution
  [s]
  (map #(mapv parseLong (string/split % #"\n")) (string/split s #"\n\n")))

(defn max-calories
  [calorie-distribution]
  (apply max (mapv #(apply + %) calorie-distribution)))

(defn top-n
  [n l]
  (vec (take-last n (sort l))))

(defn max-n-calories
  [n calorie-distribution]
  (top-n n (mapv #(apply + %) calorie-distribution)))

(defn find-max-calories
  [path]
  (let [calories-string (slurp path)
        calorie-distribution (string->calorie-distribution calories-string)]
    (max-calories calorie-distribution)))

(defn find-top-3-calories
  [path]
  (let [calories-string (slurp path)
        calorie-distribution (string->calorie-distribution calories-string)]  
      (apply + (max-n-calories 3 calorie-distribution))))

(println "Day 1 result: " (find-max-calories day-1-data-path))
(println "Day 1b result: " (find-top-3-calories day-1-data-path))

                                        ; day 2

(def day-2-data-path "resources/day-2.txt")

(def strategy-guide-map {"A" :rock "B" :paper "C" :scissor
                         "X" :rock "Y" :paper "Z" :scissor})

(def shape-score {:rock 1  :paper 2 :scissor 3})
(def round-score {:rock {:rock 3 :paper 6 :scissor 0}
                  :paper {:rock 0 :paper 3 :scissor 6}
                  :scissor {:rock 6 :paper 0 :scissor 3}})

(defn score-round
  [[opponent-move your-move]]
  (+ (shape-score your-move) (get-in round-score [opponent-move your-move])))

(defn parse-strategy-guide
  "takes the raw string with the strategy guide and returns a vector of rounds
  'A Y\nB X\nC' -> [[:rock :paper] [:paper :rock] [:scissor :scissor]]"
  [s strategy-guide-map]
  (mapv #(mapv strategy-guide-map (string/split % #" ")) (string/split s #"\n")))

(defn score-match
  [moves]
  (reduce + (map score-round moves)))
   
(defn score
  [path]
  (-> path
      slurp
      (parse-strategy-guide strategy-guide-map)
      score-match))

(println "Day 2a result: " (score day-2-data-path))

                                        ;day 2b

(def strategy-guide-map-2 {"A" :rock "B" :paper "C" :scissor
                           "X" :lose "Y" :draw "Z" :win})

; to lose to rock, we need to play scissor
(def required-move {:rock {:lose :scissor :draw :rock :win :paper}
           :paper {:lose :rock :draw :paper :win :scissor}
           :scissor {:lose :paper :draw :scissor :win :rock}})

(defn find-move
  [[move result]]
  [move (get-in required-move [move result])])

(defn strategy-guide->moves
  [strategy-guide]
  (map find-move strategy-guide))


(defn score-2
  [path]
  (-> path
      slurp
      (parse-strategy-guide strategy-guide-map-2)
      strategy-guide->moves
      score-match))

(println "Day 2b result: "(score-2 day-2-data-path))
