(ns aoc2022.core
  (:require [clojure.string :refer [split]]
            [clojure.math.combinatorics :refer [cartesian-product]]
            [clojure.test :refer [deftest testing is]]))

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

(def path "resources/day-16.txt")
(def path-test "resources/day-16-test.txt")

(def valves (prepare-input path))
(def valves-test (prepare-input path-test))

                                        ; unchecked neighbours
(defn unchecked-neighbours
  [v valves]
  (for [n (get-in valves [v :paths])
        :when (and (not (get-in valves [n :distance]))
                   (not= v n))]
    n))

                                        ; shortest path from
(defrecord Path-to [to steps]);

(defn mark-checked-valves
  [checked steps valves]
  (reduce
   (fn [valves checked-valve]
     (assoc-in valves [checked-valve :distance] steps))
   valves checked))

(defn shortest-paths-from
  [from valves]
  (loop [nexts (distinct (unchecked-neighbours from valves))
         steps 1
         valves (mark-checked-valves (list from) 1 valves)
         result '()]
    (if (empty? nexts)
      result
      (let [updated-valves (mark-checked-valves nexts steps valves)]
        (recur
         (distinct (mapcat #(unchecked-neighbours % updated-valves) nexts))
         (inc steps)
         updated-valves
         (concat result (for [n nexts
                              :when (> (get-in valves [n :flow-rate]) 0)]
                          (->Path-to n steps))))))))


                                        ; shortest paths
(defn shortest-paths
  "a list of all valves with a flow-rate > 0
  plus :AA,
  with the distances to all the other non-zero flow-rates"
  [valves]
  (into {} (for [[k v] valves
                 :when (or
                        (> (:flow-rate v) 0)
                        (= k :AA))]
             [k (shortest-paths-from k valves)])))
  
                                        ; current flow

(defn current-flow
  [valves]
  (->> valves
       (filter #(:open (val %)) ,,,)
       (map #(:flow-rate (val %)) ,,,)
       (reduce + ,,,)))

                                        ; max flow

(defn nexts
  [current-valve shortest-paths valves]
  (remove #(get-in valves [(:to %) :open]) (current-valve shortest-paths)))

(defn open-valve
  [valve valves]
  (assoc-in valves [valve :open] true))

(defn mf1
  [steps valves]
  (let [sp (shortest-paths valves)]
    (defn max-flow-1
      [current-valve steps valves]
      (if (< steps 1)
        0
        (let [nexts (nexts current-valve sp valves)]
          (if (empty? nexts)
            (* steps (current-flow valves))
            (apply max
                   (map
                    (fn [next]
                      (+ (* (min (inc (:steps next)) steps) (current-flow valves))
                         (max-flow-1
                          (:to next)
                          (- steps (:steps next) 1)
                          (open-valve (:to next) valves))))
                   nexts))))))
  (max-flow-1 :AA steps valves)))

(defrecord Event [to-valve at-step])  

(defn queue-event
  [event queue]
   (vec (sort-by :at-step (cons event queue))))

(defn peek-queue
  [queue]
  (peek queue))

(defn pop-queue
  [queue]
  (pop queue))
  
(defn nexts-to-queue
  [current-valve steps-left shortest-paths valves queue]
  (for [{:keys [to steps]} (current-valve shortest-paths)
        :let [at-step (- steps-left steps 1)]
        :when (not (get-in valves [to :open]))
        :when (> at-step 0)
        :when (not-any? #(= (:to-valve %) to) queue)]
    (->Event to at-step)))

(defn mf2m
  [steps valves]
  (let [sp (shortest-paths valves)]
       (defn max-flow-2
         [event-queue steps-left valves]
         #_(println steps-left event-queue)
         (cond
           (< steps-left 1) 0
           (empty? event-queue) (* (current-flow valves) steps-left)
           :default
           (let [{:keys [to-valve at-step]} (peek-queue event-queue)
                 nexts (nexts-to-queue to-valve at-step sp valves event-queue)]
             (+ (* (- steps-left at-step) (current-flow valves))
                (if (empty? nexts)
                  (max-flow-2
                   (pop-queue event-queue)
                   at-step
                   (open-valve to-valve valves))
                  (apply max
                         (map
                          (fn [next]
                            (max-flow-2
                             (queue-event next (pop-queue event-queue))
                             at-step
                             (open-valve to-valve valves)))
                          nexts)))))))
       (max-flow-2
        (queue-event (->Event :AA steps) (queue-event (->Event :AA steps) []))
        steps
        valves)))
