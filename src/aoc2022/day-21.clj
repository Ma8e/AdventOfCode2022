(ns aoc2022.core
  (:require [clojure.string :refer [split split-lines]]
            [clojure.test :refer [deftest is run-tests with-test]]))

(with-test
  (defn parse-monkey
    [s]
    (let [[monkey-name expression] (split s #": ")]
      (if-let [number (re-find #"\d" expression)]
        [monkey-name (bigint (Long/parseLong expression))]
        (let [[monkey1 op monkey2] (split expression #" ")]
          [monkey-name (list (read-string op) monkey1 monkey2)]))))
  (is (= ["sllz" 4] (parse-monkey "sllz: 4")))
  (is (= ["cczh" '(+ "sllz" "lgvd")] (parse-monkey "cczh: sllz + lgvd"))))

(defn parse-monkeys
  [path]
  (->> path
      slurp
      split-lines
      (map parse-monkey)
      (into {})))

(def monkeys (parse-monkeys "resources/day-21.txt"))
(def monkeys-test (parse-monkeys "resources/day-21-test.txt"))

(with-test
  (defn update-operator
    [[operator m1 m2] new-op]
    (list new-op m1 m2))
  (is (= (list = "kurt" "hub") (update-operator (list + "kurt" "hub") =))))

(defn make-i-monkeys
  [monkeys]
  (-> monkeys
      (assoc ,,, "humn" nil)
      (update ,,, "root" update-operator '-)))

(with-test 
  (defn yell
    [monkey-name monkeys]
    (if-let [expression (get monkeys monkey-name)]
      (if (number? expression)
        expression
        (let [op (first expression)
              m1 (yell (nth expression 1) monkeys)
              m2 (yell (nth expression 2) monkeys)]
          (if (every? some? (list m1 m2))
            (eval (list op m1 m2)))))))
  (let [monkeys (parse-monkeys "resources/day-21-test.txt")]
    (is (= 3 (yell "dvpt" monkeys)))
    (is (= 30 (yell "drzm" monkeys)))
    (is (= (yell "root" monkeys) 152)))
  (let [i-monkeys-test (make-i-monkeys monkeys-test)]
    (is (nil? (yell "humn" i-monkeys-test)))
    (is (nil? (yell "ptdq" i-monkeys-test)))))

(def i-monkeys-test (make-i-monkeys monkeys-test))
(def i-monkeys (make-i-monkeys monkeys))

(with-test
  (defn op-inverse
    [op]
    (case op
        + -
        - +
        * /
        / *))
  (is (= (op-inverse '+) -))
  (is (= (op-inverse '-) +))
  (is (= (op-inverse '*) /))
  (is (= (op-inverse '/) *)))

(with-test
  (defn inverse-yell
    "returns the value that nil must be replaced with for monkey in
    monkeys to have the value target"
    [monkey target monkeys]
    (if (nil? (get monkeys monkey))
      target
      (let [[op m1 m2] (get monkeys monkey)
            ym1 (yell m1 monkeys)
            ym2 (yell m2 monkeys)]
        (if (nil? ym1)
          (inverse-yell m1 ((op-inverse op) target ym2) monkeys)
          (if (#{'- '/} op)
            (inverse-yell m2 (eval (list op ym1 target)) monkeys)
            (inverse-yell m2 ((op-inverse op) target ym1) monkeys))))))
  (let [i-monkeys-test (make-i-monkeys monkeys-test)]
    (is (= (inverse-yell "humn" 7 i-monkeys-test) 7))
    (is (= (inverse-yell "ptdq" 8 i-monkeys-test) 11) "(- nil 3) = 8 -> nil = 11")
    (is (= (inverse-yell "root" 0 i-monkeys-test) 301)))
  (is (= (inverse-yell "a" 12 {"a" (list '/ "b" "c") "b" nil "c" 3}) 36))
  (is (= (inverse-yell "a" 4 {"a" (list '/ "b" "c") "b" 12 "c" nil}) 3)))

(def answer-day-21-part-1 (yell "root" monkeys))
(def answer-day-21-part-2 (inverse-yell "root" 0 i-monkeys))

(run-tests)
