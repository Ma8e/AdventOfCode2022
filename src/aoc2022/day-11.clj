(ns aoc2022.core)


(defn divisible?
  [n d]
  (= 0 (rem n d)))

(defn round-down
  [n]
  (long (Math/floor n)))

(def monkeys-test
  (mapv #(assoc % :inspected-items 0)
  [{:monkey-no 0
    :items [79 98]
    :operation #(* % 19)
    :test #(divisible? % 23)
    :recipients [2 3]}
   {:monkey-no 1
    :items [54, 65, 75, 74]
    :operation #(+ % 6)
    :test #(divisible? % 19)
    :recipients [2 0]}
   {:monkey-no 2
    :items [79, 60, 97]
    :operation #(* % %)
    :test #(divisible? % 13)
    :recipients [1 3]}
   {:monkey-no 3
    :items [74]
    :operation #(+ % 3)
    :test #(divisible? % 17)
    :recipients [0 1]}                   
   ]))

(def monkeys
  (mapv
   #(assoc % :inspected-items 0)
   [{:monkey-no 0
     :items [83, 97, 95, 67]
     :operation #(* 19 %)
     :test #(divisible? % 17)
     :recipients [2 7]}
    {:monkey-no 1
     :items [71, 70, 79, 88, 56, 70]
     :operation #(+ 2 %)
     :test #(divisible? % 19)
     :recipients [7 0]}
    {:monkey-no 2
     :items [98, 51, 51, 63, 80, 85, 84, 95]
     :operation #(+ 7 %)
     :test #(divisible? % 7 )
     :recipients [4 3]}
    {:monkey-no 3
     :items [77, 90, 82, 80, 79]
     :operation #(+ 1 %)
     :test #(divisible? % 11)
     :recipients [6 4]}
    {:monkey-no 4
     :items [68]
     :operation #(* 5 %)
     :test #(divisible? % 13)
     :recipients [6 5]}
    {:monkey-no 5
     :items [60 94]
     :operation #(+ 5 %)
     :test #(divisible? % 3)
     :recipients [1 0]}
    {:monkey-no 6
     :items [81, 51, 85]
     :operation #(* % %)
     :test #(divisible? % 5)
     :recipients [5 1]}
    {:monkey-no 7
     :items [98, 81, 63, 65, 84, 71, 84]
     :operation #(+ 3 %)
     :test #(divisible? % 2) 
     :recipients [2 3]}
   ]))

(def lcm (apply * '(2 3 5 7 11 13 17 19)))
;(def lcm (* 13 17 19 23))

(defn receive-item
  [monkey item]
  (update monkey :items #(conj % item)))

(defn throw-item
  [monkey]
  (-> monkey
      (update ,,, :items #(vec (rest %)))
      (update ,,, :inspected-items inc)))

(defn inspect-and-throw-item
  "always inspect the first item"
  [monkey-no monkeys]
  (let [{:keys [operation test recipients items]} (nth monkeys monkey-no)
        ;new-worry-level (round-down (/ (operation (first items)) 3))
        new-worry-level (rem (operation (first items)) lcm)
        receiver (if (test new-worry-level) (first recipients) (second recipients))]
    #_(println (operation (first items)) new-worry-level)
    (-> monkeys
        (update ,,, monkey-no #(throw-item %))
        (update ,,, receiver #(receive-item % new-worry-level)))))
        
(defn take-turn
  [monkey-no monkeys]
  (if (empty? (:items (nth monkeys monkey-no)))
    monkeys
    (take-turn monkey-no (inspect-and-throw-item monkey-no monkeys))))

(defn round
  "each monkey take a turn"
  [monkeys]
  (loop [monkey-no 0
         monkeys monkeys]
    (if (>= monkey-no (count monkeys))
      monkeys
      (recur (inc monkey-no)
             (take-turn monkey-no monkeys)))))

(defn run
  [times monkeys]
  (if (< times 1)
    monkeys
    (recur (dec times) (round monkeys))))

(defn max-n
  [n coll]
  (take n (sort > coll)))
  
;(println "Day 11a result: " (reduce * (max-n 2 (map :inspected-items (run 20 monkeys)))))
(reduce * (max-n 2 (map :inspected-items (run 10000 monkeys))))
