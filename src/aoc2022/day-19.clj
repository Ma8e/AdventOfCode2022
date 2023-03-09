(ns aoc2022.core
  (:require [clojure.string :refer [split]]))

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

(def blueprints-test (parse-blueprints (slurp "resources/day-19-test.txt")))
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

(defn resources-for-robot?
  [robot blueprint resources]
     (every?
      (fn [[resource c]] (>= c (get-in blueprint [robot resource] 0)))
      resources))

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
      (->> robot-types
           (filter #(resources-for-robot? % blueprint (:resources state)))
           (remove #(enough-of-robots? % state blueprint time-left))
           (cons :none)
           ((fn [rt] (if (some #{:geode} rt) '(:geode) rt))))))
          
(defn inc-state-time
  [state]
  (update state :time inc))

(def initial-state
  {:time 1
   :resources {:ore 0 :clay 0 :obsidian 0 :geode 0}
   :robots {:ore 1 :clay 0 :obsidian 0 :geode 0}})


                                        ; utan memoization
(defn max-geodes
  [state blueprint max-time]
  (if (> (:time state) max-time)
    (get-in state [:resources :geode])
    (let [build-candidates
          (build-candidates state blueprint max-time)]
      (apply max
             (for [robot build-candidates]
               (max-geodes
                (->> state
                     mine
                     inc-state-time
                     (build-robot robot blueprint))
                blueprint
                max-time))))))

                                        ; med memoization 
(defn max-geodes-memoized
  [blueprint max-time]
  (let [cache (atom {})
        hitstat (atom {:hit 0 :miss 0})]
    (letfn [(mg [state]
      (if-let [r (get @cache state)]
          (do
            (swap! hitstat update :hit inc)
            r)
          (let [result 
                (if (> (:time state) max-time)
                  (do
                    (get-in state [:resources :geode]))
                  (let [build-candidates
                        (build-candidates state blueprint max-time)]
                    (apply max
                           (for [robot build-candidates]
                             (mg
                              (->> state
                                   mine
                                   inc-state-time
                                   (build-robot robot blueprint)))))))]
            (swap! cache assoc state result)
            (swap! hitstat update :miss inc)
            result)))]
      [(mg initial-state) @hitstat])))

                                        ; med pruning och memoization

(defn upper-bound
  [state max-time]
  (let [time-left (- max-time (:time state))]
    (+ (get-in state [:resources :geode])
       (* (get-in state [:robots :geode]) (inc time-left))
       (/ (* time-left (inc time-left)) 2))))

(defn max-geodes-pruned
  [blueprint max-time]
  (let [cache (atom {})
        best (atom 0)
        hitstat (atom {:hit 0 :miss 0 :pruned 0})]
    (letfn [(mg [state]
              (if-let [r (get @cache state)]
                (do
                  (swap! hitstat update :hit inc)
                  r)
                (if (< (upper-bound state max-time) @best)
                  (do
                    (swap! hitstat update :pruned inc)
                    0)
                  (let [result 
                        (if (> (:time state) max-time)
                          (do
                            (get-in state [:resources :geode]))
                          (let [build-candidates
                                (build-candidates state blueprint max-time)]
                            (apply max
                                   (for [robot build-candidates]
                                     (mg
                                      (->> state
                                           mine
                                           inc-state-time
                                           (build-robot robot blueprint)))))))]
                    (swap! best max result)
                    (swap! cache assoc state result)
                    (swap! hitstat update :miss inc)
                    result))))]
      [(mg initial-state) @hitstat])))

(defn quality-level
  [blueprint]
  (*
   (:blueprint blueprint)
   (max-geodes-memoized blueprint 24)))

(defn quality-score
  [blueprints]
  (apply + (pmap quality-level blueprints)))
         
