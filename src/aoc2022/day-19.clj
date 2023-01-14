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

(def resources-for-robot?
;  (memoize
   (fn [robot blueprint resources]
     (every?
      (fn [[resource c]] (>= c (get-in blueprint [robot resource] 0)))
      resources)))

(defn build-robot
  [robot blueprint robots resources]
  [(update robots robot inc)
   (into {} (for [[resource c] resources]
              [resource (- c (get-in blueprint [robot resource] 0))]))])

(defn mine
  [robots resources]
  (reduce
   (fn [resources resource-type]
     (update resources resource-type #(+ % (resource-type robots))))
   resources
   (keys robots)))


(defn max-geodes-recurse
   [robots resources blueprint time max-time]
            #_(println robots resources time)
     (if (> time max-time)
       (:geode resources)
       (let [build-candidates
             (filter #(resources-for-robot? % blueprint resources) (keys robots))
             builds (map #(build-robot % blueprint robots resources) build-candidates)
             builds (if (or (and (zero? (:clay robots))
                                 (every?
                                  #(resources-for-robot? % blueprint resources)
                                  '(:ore :clay)))
                            (and (zero? (:obsidian robots))
                                 (every?
                                  #(resources-for-robot? % blueprint resources)
                                  '(:ore :clay :obsidian)))
                            (every?
                             #(resources-for-robot? % blueprint resources)
                             (keys robots)))
                      builds
                      (conj builds [robots resources]))]                                        
         (apply max
                (for [[robs res] builds]
                  (max-geodes-recurse
                   robs
                   (mine robots res)
                   blueprint
                   (inc time)
                   max-time))))))

(defn max-geodes
  [blueprint minutes]
  (max-geodes-recurse
   {:ore 1 :clay 0 :obsidian 0 :geode 0}
   {:ore 0 :clay 0 :obsidian 0 :geode 0}
   blueprint
   0
   minutes))


                      

    
         
