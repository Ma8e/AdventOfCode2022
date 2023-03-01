(ns aoc2022.core
  (:require [clojure.string :refer [split]]))

(def blueprint-string-test "Blueprint 1:  Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.\nBlueprint 2:  Each ore robot costs 2 ore.  Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.")


(def resource-types '(:ore :clay :obsidian :geode))
(def robot-types resource-types)

(defn find-max-costs
  [blueprint]
  (reduce
   (fn [max-costs robot-type]
     (assoc max-costs robot-type (apply max (remove nil? (map robot-type (vals blueprint))))))
   {}
   '(:ore :clay :obsidian)))

(defn parse-blueprint
  [s]
  (let [[blueprint-nr
         ore-robot-ore-cost
         clay-robot-ore-cost
         obsidian-robot-ore-cost
         obsidian-robot-clay-cost
         geode-robot-ore-cost
         geode-robot-obsidian-cost] (map #(Long/parseLong %) (re-seq #"\d+" s))
        blueprint {
                   :blueprint blueprint-nr
                   :ore {:ore ore-robot-ore-cost}
                   :clay {:ore clay-robot-ore-cost}
                   :obsidian {:ore obsidian-robot-ore-cost :clay obsidian-robot-clay-cost}
                   :geode {:ore geode-robot-ore-cost :obsidian geode-robot-obsidian-cost}}]
    (assoc blueprint :max-costs (find-max-costs blueprint))))

(defn parse-blueprints
  [s]
  (mapv parse-blueprint (split s #"\n")))

(def blueprints-test (parse-blueprints blueprint-string-test))

(def blueprints (parse-blueprints (slurp "resources/day-19.txt")))


(defn subtract-map
  [m1 m2]
  (reduce
   (fn [result [k v]] (conj result [k (- v (get m2 k 0))]))
   {}
   m1))

(defn build-robot
  [robot blueprint state]
  (if (= robot :none)
    state
    (-> state
        (update-in ,,, [:robots robot] inc)
        (update ,,, :resources subtract-map (robot blueprint)))))

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

(def resources-for-robot?
   (fn [robot blueprint resources]
     (every?
      (fn [[resource c]] (>= c (get-in blueprint [robot resource] 0)))
      resources)))

(defn enough-of-robots?
  [robot state blueprint time-left]
  (and 
   (not= robot :geode) ;; we never get enough of :geodes
   (> (+ (* time-left
            (- (get-in state [:robots robot])
               (get-in blueprint [:max-costs robot])))
         (get-in state [:resources robot]))
      0)))

(defn build-candidates
  [state blueprint max-time]
  (let [time-left (- max-time (:time state))]
    (if (= time-left 1)
      '(:none)
      (->> robot-types
           (filter #(resources-for-robot? % blueprint (:resources state)))
           (remove #(enough-of-robots? % state blueprint time-left))
           (cons :none)
           ((fn [rt] (if (some #{:geode} rt) '(:geode) rt)))))))
          
(defn inc-state-time
  [state]
  (update state :time inc))

(defn incr-state
  [robot blueprint state]
  ((comp
    (partial build-robot robot blueprint)
    inc-state-time
    mine)
   state))
  
(def max-geodes
  (memoize 
   (defn mg
     [state blueprint max-time]
     (if (> (:time state) max-time)
       (get-in state [:resources :geode])
       (let [build-candidates
             (build-candidates state blueprint max-time)]
         #_(println (:time state) build-candidates (:robots state))
         (apply max
                (for [robot build-candidates]
                  (max-geodes
                   (->> state
                        mine
                        inc-state-time
                        (build-robot robot blueprint))
                   blueprint
                   max-time))))))))
 
(def initial-state
  {:time 1
   :resources {:ore 0 :clay 0 :obsidian 0 :geode 0}
   :robots {:ore 1 :clay 0 :obsidian 0 :geode 0}})

(defn quality-level
  [blueprint]
  (*
   (:blueprint blueprint)
   (max-geodes initial-state blueprint 24)))

(defn quality-score
  [blueprints]
  (apply + (pmap quality-level blueprints)))


                      

    
         
