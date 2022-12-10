(ns aoc2022.core
  (:require [clojure.string :refer [split]]))

;(def instructions-path "resources/day-10-test.txt")
(def instructions-path "resources/day-10.txt")

(defn token-one-instruction
  [s]
  (let [raw (split s #" ")]
    (case (first raw)
    "addx" ["addx" (Integer/parseInt (second raw))]
    "noop" ["noop"])))

(defn token-instructions
  [s]
  (mapv #(token-one-instruction %) (split s #"\n")))

(def instructions (token-instructions (slurp instructions-path)))

(def register-init-state
  {:blocked 0
   :V 0
   :X 1
   :instr nil
   :instr-i 0})
  
(defn update-registers
  [{:keys [blocked V X instr instr-i] :as registers} instructions]
  (if (> blocked 0)
    (assoc registers :blocked (dec blocked))
    (let [[new-instruction new-V] (nth instructions instr-i)]
      (assoc
       registers
       :X (+ X (if (= instr "addx") V 0))
       :blocked (case new-instruction "addx" 1 "noop" 0)
       :instr-i (inc instr-i)
       :V new-V
       :instr new-instruction))))

(defn prettify-screen
  [screen]
  (map (partial apply str) (partition 40 screen)))

(defn process
  [instructions]
  (let [no-of-instructions (count instructions)]
    (loop [registers (update-registers register-init-state instructions)
           X-log  []]
      (if (>= (:instr-i registers) no-of-instructions)
        (conj X-log (:X registers))
        (recur (update-registers registers instructions)
               (conj X-log (:X registers)))))))

(def X-log (process instructions))

;; sum of signal strenghts (part 1)
(reduce +  (map #(* (nth X-log (dec %)) %) [20 60 100 140 180 220]))

;; CRT (part 2)
(def screen (prettify-screen
 (map (fn [X screen-pos]
        (if (<= (dec X) (mod screen-pos 40) (inc X))
          \X
          \.))
      X-log (range 0 240))))
