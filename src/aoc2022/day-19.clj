(ns aoc2022.core
  (:require [clojure.string :refer [split]]))

(def blueprint-string-test "Blueprint 1:  Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.\nBlueprint 2:  Each ore robot costs 2 ore.  Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.")

(defn parse-blueprint
  [s]
  (let [[blueprint
         ore-robot-ore-cost
         clay-robot-ore-cost
         obsidian-robot-ore-cost
         obsidian-robot-clay-cost
         geode-robot-ore-cost
         geode-robot-obsidian-cost] (map #(Long/parseLong %) (re-seq #"\d+" s))]
    {:blueprint blueprint
     :ore {:ore ore-robot-ore-cost}
     :clay {:ore clay-robot-ore-cost}
     :obsidian {:ore obsidian-robot-ore-cost :clay obsidian-robot-clay-cost}
     :geode {:ore geode-robot-ore-cost :obsidian geode-robot-obsidian-cost}}))

(def empty-resources (zipmap [:ore :clay :obsidian :geode] (repeat 0)))

(defn parse-blueprints
  [s]
  (mapv parse-blueprint (split s #"\n")))

(def blueprints-test (parse-blueprints blueprint-string-test))

(defn resources-for-robot?
  [robot blueprint resources]
  (every?
   (fn [[resource c]] (>= c (get-in blueprint [robot resource] 0)))
   resources))

(defn subtract-map
  [m1 m2]
  (reduce
   (fn [result [k v]] (conj result [k (- v (get m2 k 0))]))
   {}
   m1))

(defn build-robot
  [robot blueprint state]
  (println \newline "Building " robot)
  (-> state
      (update-in ,,, [:robots robot] inc)
      (update ,,, :resources subtract-map (robot blueprint))))

(defn mine
  [state]
  (reduce
   (fn [state resource-type]
     (update-in
      state
      [:resources resource-type]
      #(+ % (get-in state [:robots resource-type]))))
    state
    (keys (:robots state))))

(defn key-for-max-value
  [m]
  (if (= 1 (count m))
    (first (keys m))
    (reduce (fn [[k1 v1] [k2 v2]] (if (> v1 v2) k1 k2)) m)))


(defn choose-build
  [resources blueprint]
  (letfn [(choose-build-recurse [robot]
            (if (resources-for-robot? robot blueprint resources)
              robot
              (if (= robot :ore)
                nil
                (choose-build-recurse
                 (key-for-max-value
                  (subtract-map
                   (robot blueprint)
                   resources))))))]
    (choose-build-recurse :geode)))

(defn step-time
  [state]
  (update state :minute inc))

(defn max-geodes
  [blueprint]
  (let [initial-state
        {:minute 0
         :robots {:ore 1 :clay 0 :obsidian 0 :geode 0}
         :resources {:ore 0 :clay 0 :obsidian 0 :geode 0}}]
    (letfn [(m-g
              [state]
              (step-time 
               (if-let [robot-to-build (choose-build (:resources state) blueprint)]
                 (build-robot
                  robot-to-build
                  blueprint
                  (mine state))
                 (mine state))))]
      (iterate m-g initial-state))))

(take 24 (map (fn [a b] [a b]) (range) (max-geodes (first blueprints-test))))




                      

    
         
