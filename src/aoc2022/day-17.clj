(ns aoc2022.core
  (:require [clojure.string :refer [join]]
            [clojure.test :refer [deftest testing is are run-tests]]))

(def rocks (vector
            (for [x (range 4)] [x 0])
            (list [1 2] [0 1] [1 1] [2 1] [1 0])
            (list [2 2] [2 1] [0 0] [1 0] [2 0])
            (for [y (range 4)] [0 y])
            (list [0 1] [1 1] [0 0] [1 0])))

(def jet-stream-test ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")

(def jet-stream (slurp "resources/day-17.txt"))

(defn print-rock
  [rock]
  (println
   (for [y (range 4 -1 -1)
         x (range 6)]
     (cond
       (some #(= [x y] %) rock) \#
       (= x 5) \newline
       :default \.))))

(def chamber (vec (repeat 9000 (vec (repeat 7 \.)))))

(defn contains-rock?
  [row]
  (some #(= \# %) row))


(defn top-rock
  [chamber]
  (let [tr (last (for [y (range (count chamber))
                 :while (contains-rock? (get chamber y))]
                   y))]
    (if tr tr -1)))

(defn chamber-y-range
  [chamber]
  (range (+ 5 (top-rock chamber)) -1 -1))

(defn chamber-point
  [[x y] chamber]
  (if (= x 7)
    \newline
    (str
     (if (zero? x)
       (format "%5d " y)
       nil)
     (get-in chamber [y x]))))

(defn print-chamber
  [chamber]
  (println
   (for [y (chamber-y-range chamber)
         x (range 8)]
     (chamber-point [x y] chamber))))

(defn add-rock
  [rock chamber]
  (if (empty? rock)
    chamber
    (recur
     (rest rock)
     (assoc-in chamber (reverse (first rock)) \#))))

(defn add-coordinate
  [coor rock]
  (map #(mapv + % coor) rock))

(defn outside-chamber?
  [rock]
  (some #(or (< (first %) 0)
             (> (first %) 6))
        rock))

(defn rock-collision?
  [rock chamber]
  (some #(= (get-in chamber (reverse %)) \#) rock))

(defn jet-rock
  "move one step with the wind if possible"
  [rock jet chamber]
  (let [jetted-rock (add-coordinate
                     (if (= jet \<)
                       [-1 0]
                       [1 0])
                     rock)]
    (if (or (outside-chamber? jetted-rock)
            (rock-collision? jetted-rock chamber))
      rock
      jetted-rock)))

(defn fall-rock
  "move one step with gravity if possible"
  [rock chamber]
  (let [fallen-rock (add-coordinate [0 -1] rock)]
    (if (or (some #(< (second %) 0) fallen-rock)
            (rock-collision? fallen-rock chamber))
      rock
      fallen-rock)))
                   
  
(defn drop-rock
  [rock jet-stream chamber top-rock]
  #_(println "top-rock" (top-rock chamber))
  (loop [rock (add-coordinate [2 (+ top-rock 4)] rock)
         jet-stream jet-stream]
    (let [jetted-rock (jet-rock rock (first jet-stream) chamber)
          fallen-rock (fall-rock jetted-rock chamber)]
      #_(println "Rock" rock (take 10 jet-stream))
      #_(print-chamber (add-rock rock chamber))
      #_(println "jetted rock" jetted-rock)
      #_(print-chamber (add-rock jetted-rock chamber))
      #_(println "fallen-rock" fallen-rock)
      #_(print-chamber (add-rock fallen-rock chamber))
      (if (= fallen-rock jetted-rock)
        [fallen-rock (rest jet-stream)]
        (recur fallen-rock (rest jet-stream))))))

(def c (atom 0))

(defn rf
  [rocks jet-pattern max-rocks]
  (loop [rocks (apply concat (repeat rocks))
         jp (apply concat (repeat (seq jet-pattern)))
         chamber (vec (repeat (* max-rocks 3) (vec (repeat 7 \.))))
         nr-rocks-dropped 0
         top-rock -1]
    (if (>= nr-rocks-dropped max-rocks)
      (do
        (reset! c (subvec chamber 0 (inc top-rock)))
        #_(print-chamber chamber)
        (inc top-rock))
      (let [[dropped-rock remaining-jet-stream] (drop-rock (first rocks) jp chamber top-rock)] 
        (recur
         (rest rocks)
         remaining-jet-stream
         (add-rock dropped-rock chamber)
         (inc nr-rocks-dropped)
         (apply max top-rock (map second dropped-rock)))))))

(def m {\. \0 \# \1})
   
(defn row->number
  [row]
  (Long/parseLong (apply str (map m row)) 2))

(defn cycle-length
  [v]
  (if (empty? v)
    0
    (let [l (count v)]
      (apply min (map count 
                      (for [cl (range  1 (inc l))
                            :let [candidate (subvec v 0 cl)]
                            :when (= v (take l (flatten (repeat candidate))))]
                        candidate))))))   

(deftest test-cycle-length
  (are [expected v] (= expected (cycle-length v))
    0 []
    1 [7]
    2 [1 2]
    2 [1 2 1]
    2 [1 2 1 2 1]
    2 [1 2 1 2 1 2]
    3 [1 2 3]
    3 [1 2 3 1]
    3 [1 2 3 1 2]
    3 [1 2 3 1 2 3]
    6 [1 2 3 1 2 4]
    7 [1 2 3 1 2 3 4]))         


