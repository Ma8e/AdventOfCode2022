(ns aoc2022.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

                                        ; day 1

(def day-1-data-path "resources/day-1.txt")

(defn parseLong [s] (Long/parseLong s)) ; because parseLong isn't static?

(defn string->calorie-distribution
  [s]
  (map #(mapv parseLong (string/split % #"\n")) (string/split s #"\n\n")))

(defn max-calories
  [calorie-distribution]
  (apply max (mapv #(apply + %) calorie-distribution)))

(defn top-n
  [n l]
  (vec (take-last n (sort l))))

(defn max-n-calories
  [n calorie-distribution]
  (top-n n (mapv #(apply + %) calorie-distribution)))

(defn find-max-calories
  [path]
  (let [calories-string (slurp path)
        calorie-distribution (string->calorie-distribution calories-string)]
    (max-calories calorie-distribution)))

(defn find-top-3-calories
  [path]
  (let [calories-string (slurp path)
        calorie-distribution (string->calorie-distribution calories-string)]  
      (apply + (max-n-calories 3 calorie-distribution))))

(println "Day 1 result: " (find-max-calories day-1-data-path))
(println "Day 1b result: " (find-top-3-calories day-1-data-path))


