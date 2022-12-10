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

(defn update-screen
  [screen cycle-no X]
  (str
   screen
   (if (contains? #{(dec X) X (inc X)} (rem (dec cycle-no) 40))
     "#"
     ".")))

(defn prettify-screen
  [screen]
  (map (partial apply str) (partition 40 screen)))

(defn process
  [instructions]
  (let [no-of-instructions (count instructions)
        checkpoints #{20 60 100 140 180 220}]
    (loop [cycle-no 1
           registers (update-registers register-init-state instructions)
           checkpoint-signal-strength-sum 0
           screen ""]
      (if (>= (:instr-i registers) no-of-instructions)
        [checkpoint-signal-strength-sum
         (prettify-screen (update-screen screen cycle-no (:X registers)))]
        (recur (inc cycle-no)
               (update-registers registers instructions)
               (if (contains? checkpoints cycle-no)
                 (+ checkpoint-signal-strength-sum (* cycle-no (:X registers)))
                 checkpoint-signal-strength-sum)
               (update-screen screen cycle-no (:X registers)))))))
