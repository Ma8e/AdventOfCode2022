(ns aoc2022.core
  (:require [clojure.string :refer [split]]
            [clojure.set :refer [difference union]]
            [clojure.test :refer [deftest testing is run-tests]]))

(def path-test "resources/day-16-test.txt")
(def path "resources/day-16.txt")

(defn parse-line
  [s]
  (let [[c & out] (re-seq #"[A-Z]{2}" s)
        flow-rate (re-find #"\d+" s)]
    {(keyword c) {:paths (map keyword out) :flow-rate (Long/parseLong flow-rate) :open false}}))

(defn prepare-input
  [path]
  (into {} (map parse-line (-> path
                               slurp
                               (split #"\n")))))

(def valves-test-aoc (prepare-input path-test))
(def valves (prepare-input path))

(defn release-rate
  [valves]
  (apply + (map :flow-rate (filter :open (vals valves)))))

(def m-max-release (memoize (defn max-release
  [time-left start-position valves]
  (if (< time-left 1)
    0
    (+ (release-rate valves)
       (apply max
              (if (or (get-in valves [start-position :open])
                      (= (get-in valves [start-position :flow-rate]) 0))
                      0
                (m-max-release
                 (dec time-left)
                 start-position
                 (assoc-in valves [start-position :open] true)))
              (map
               (fn [next-valve]
                 (m-max-release
                  (dec time-left)
                  next-valve
                  valves))
               (get-in valves [start-position :paths])))
       )))))


;; p is not [current-p previous-p]

(defn next-action-options
  ([p valves p2]
   (let [{:keys [paths flow-rate open]} ((:current p) valves)
         options (remove #(or (nil? %) (= (:previous p) %))
             (conj 
              paths
              (if (and (> flow-rate 0) (not open) (not= (:current p) p2))
                (:current p)
                nil)))]
     (if (empty? options)
       (list (:current p))
       options
     )))
  ([p valves]
   (next-action-options p valves nil)))

(defn open-valve
  [v valves]
  (assoc-in valves [v :open] true))

(def m-max-release-2
  (memoize
   (defn max-release-2
     [time-left p valves]
     (if (< time-left 1)
       0
       (let [[p1 p2] (sort #(compare (:current %1) (:current %2)) p) 
             p1-options (next-action-options p1 valves)
             p2-options (next-action-options p2 valves (:current p1))]
         (+ (release-rate valves)
            (apply max
                   (for [np1 p1-options
                         np2 p2-options]
                     (m-max-release-2
                      (dec time-left)
                      [{:current np1 :previous (:current p1)}
                       {:current np2 :previous (:current p2)}]
                      (->> valves
                           (#(if (= np1 (:current p1)) (open-valve (:current p1) %) %))
                           (#(if (= np2 (:current p2)) (open-valve (:current p2) %) %))))))))))))



(defn shortest-paths-to-pos-flow-valves
  [start valves] 
  (loop
    [steps 1
     nexts (get-in valves [start :paths])
     shortests {}
     visited #{start}]
    (if (empty? nexts)
      shortests
      (let [encountered-non-zero-flow-valves (filter #(> (get-in valves [% :flow-rate]) 0) nexts)
            next-neighbours (reduce union (map #(set (get-in valves [% :paths])) nexts))]
        (recur
         (inc steps)
         (difference next-neighbours visited)
         (reduce merge shortests (map #(hash-map % steps) encountered-non-zero-flow-valves))
         (into visited nexts))))))
         
    
(defn shortest-path-between-non-zero-valves
  [valves]
  (let [not-zero-rate-valves (keys (filter #(> (:flow-rate (val %)) 0) valves))]
    (reduce merge
            (map
             #(hash-map % (shortest-paths-to-pos-flow-valves % valves))
             not-zero-rate-valves))))

(def
  shortest-paths
  (memoize
   (fn
     [valves]
     (assoc
      (shortest-path-between-non-zero-valves valves)
      :AA (shortest-paths-to-pos-flow-valves :AA valves)))))
                          
(defn open?
  [v valves]
  (get-in valves [v :open]))

(def m-max-release-faster
  (memoize
   (defn max-release-faster
     [time-left position travel-time valves sp]
     (let [nexts (remove #(open? (key %) valves) (position sp))
           updated-valves (open-valve position valves) ]
       (if (<= (- time-left travel-time) 1)
         (* time-left (release-rate valves))
         (+ (* (inc travel-time) (release-rate valves))
            (if (empty? nexts)
              (* (release-rate updated-valves) (- time-left travel-time 1))
              (apply max
                     (map
                      #(m-max-release-faster
                        (- time-left travel-time 1)
                        (key %)
                        (val %)
                        updated-valves
                        sp)
                      nexts)))))))))

(defn mr
  [time valves]
  (let [sp (shortest-paths valves)]
    (m-max-release-faster (inc time) :AA 0 valves sp)))

(defn mrh
  [time valves]
  (let [sp (shortest-paths valves)]
    (m-max-release-faster-history (inc time) :AA 0 valves sp)))


(def valves-test-1 ;; 
  {:AA {:paths '() :flow-rate 2 :open false}})

(deftest valves-1-test
  (testing "Only one valve with flow rate 2"
    (testing ",travel time is 0"
      (is (= 0 (m-max-release-faster 1 :AA 0 valves-test-1)))
      (is (= 2 (m-max-release-faster 2 :AA 0 valves-test-1)))
      (is (= 4 (m-max-release-faster 3 :AA 0 valves-test-1)))))
    (testing ",travel time is 2"
      (is (= 0 (m-max-release-faster 3 :AA 2 valves-test-1)))
      (is (= 2 (m-max-release-faster 4 :AA 2 valves-test-1)))
      (is (= 14 (m-max-release-faster 10 :AA 2 valves-test-1))))
    (testing ", travel is longer than time left"
      (is (= 0 (m-max-release-faster 2 :AA 4 valves-test-1)))
      (is (= 0 (m-max-release-faster 2 :AA 1 valves-test-1)))
      (is (= 0 (m-max-release-faster 2 :AA 2 valves-test-1)))))

(def valves-test-2
  {:AA {:paths '(:BB) :flow-rate 2 :open false}
   :BB {:paths '(:AA) :flow-rate 3 :open false}})

(deftest valves-2-test
  (is (= 0 (m-max-release-faster 2 :AA 2 valves-test-2)) "time-left=2, travel-time=2")
  (is (= 0 (m-max-release-faster 3 :AA 2 valves-test-2)))
  (is (= 2 (m-max-release-faster 4 :AA 2 valves-test-2)))
  (is (= 4 (m-max-release-faster 5 :AA 2 valves-test-2)))
  (is (= 9 (m-max-release-faster 6 :AA 2 valves-test-2)))
  (is (= 14 (m-max-release-faster 7 :AA 2 valves-test-2))))

(def valves-test-3
  {:AA {:paths '(:BB) :flow-rate 2 :open false}
   :BB {:paths '(:AA :CC) :flow-rate 0 :open false}
   :CC {:paths '(:BB) :flow-rate 3 :open false}})

(deftest valves-3-test
  (is (= 0 (m-max-release-faster 0 :AA 0 valves-test-3)))
  (is (= 0 (m-max-release-faster 1 :AA 0 valves-test-3)))
  (is (= 2 (m-max-release-faster 2 :AA 0 valves-test-3)))
  (is (= 4 (m-max-release-faster 3 :AA 0 valves-test-3)))
  (is (= 6 (m-max-release-faster 4 :AA 0 valves-test-3)))
  (is (= 11 (m-max-release-faster 5 :AA 0 valves-test-3)))
  (is (= 16 (m-max-release-faster 6 :AA 0 valves-test-3)))
  (is (= 21 (m-max-release-faster 7 :AA 0 valves-test-3))))

(deftest aoc-test
  (is (= 1651 (m-max-release-faster 31 :AA 0 valves-test-aoc))))


