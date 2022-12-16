(ns advent2022.day11
  (:require [advent2022.utils :refer [map-vals]]
            [clojure.core.match :refer [match]]
            [clojure.string :refer [blank? split split-lines trim]]))

(defn load-input [] (slurp "resources/day11.in"))

(def monkey-id-re #"^\s*Monkey (\d+):\s*$")

(def starting-items-re #"^\s*Starting items:\s*((:?\d+,?\s*)+)$")

(def operation-re #"^\s*Operation: new = ((?:old)|(?:\d+))\s*(\+|\*)\s*((?:old)|(?:\d+))$")

(def test-re #"^\s*Test: divisible by (\d+)\s*$")

(def test-branch-re #"^\s*If ((?:true)|(?:false)): throw to monkey (\d+)\s*$")

(defn parse-starting-items [line]
  (if-let [[_ items-str] (re-matches starting-items-re line)]
    (let [items (split items-str #",\s*")]
      (map #(Integer/parseInt %) items))
    (throw (AssertionError. (str "'" line "' does not match starting-items regex")))))

(defn parse-monkey-id [line]
  (if-let [[_ id] (re-matches monkey-id-re line)]
    id
    (throw (AssertionError. (str "'" line "' does not match monkey-id regex")))))

(defn resolve [operand old]
  (if (= (:type operand) :number)
    (:val operand)
    old))

(defn make-operator [f l r]
  (fn [old]
    (let [lhs (resolve l old)
          rhs (resolve r old)]
      (f lhs rhs))))

(defn parse-operand [operand]
  (if (= operand "old")
    {:type :old}
    {:type :number :val (Integer/parseInt operand)}))

(defn op-to-fn [op]
  (match op
    "+" +
    "*" *))

(defn parse-operation [line]
  (if-let [[_ lhs op rhs] (re-matches operation-re line)]
    (let [op (op-to-fn op)
          lhs (parse-operand lhs)
          rhs (parse-operand rhs)]
      (make-operator op lhs rhs))
    (throw (AssertionError. (str "'" line "' does not match operation regex")))))

(defn parse-condition [line]
  (if-let [[_ div-by] (re-matches test-re line)]
    (Integer/parseInt div-by)
    (throw (AssertionError. (str "'" line "' does not match test regex")))))

(defn parse-test-branch [line expected]
  (if-let [[_ branch to-monkey] (re-matches test-branch-re line)]
    (if (= branch (str expected))
      to-monkey
      (throw (AssertionError. (str "expected branch for " expected " but got branch for " branch))))
    (throw (AssertionError. (str "'" line "' does not match test-branch regex")))))

(defn divisible-by [a b]
  (zero? (mod a b)))

(defn parse-test [condition when-true when-false]
  (let [div-by (parse-condition condition)
        true-monkey (parse-test-branch when-true true)
        false-monkey (parse-test-branch when-false false)]
    [(fn [value]
       (if (divisible-by value div-by)
         true-monkey
         false-monkey))
     div-by]))

(defn parse-monkey [lines]
  (if (< (count lines) 6)
    (throw (AssertionError. (str "need 6 lines to parse a monkey but got " (count lines))))
    (let [[id-line start-items-line operation-line
           cond-line true-branch-line false-branch-line] lines
          id (parse-monkey-id id-line)
          start-items (parse-starting-items start-items-line)
          operation (parse-operation operation-line)
          [test test-prime] (parse-test cond-line true-branch-line false-branch-line)]
      {:id id
       :items start-items
       :operation operation
       :test test
       :inspected 0
       :test-prime test-prime})))

(defn parse-input [input]
  (let [lines (filter (complement blank?)
                      (map trim (split-lines input)))
        monkeys (map parse-monkey (partition 6 lines))
        id-order (map :id monkeys)
        test-prime-product (apply * (map :test-prime monkeys))]
    [(into {} (map (fn [monkey] [(:id monkey) monkey]) monkeys)) id-order test-prime-product]))

(defn sim-item-p1 [monkey worry-level]
  (let [during-inspection ((:operation monkey) worry-level)
        post-inspection (int (/ during-inspection 3))
        next-monkey ((:test monkey) post-inspection)]
    {:to-monkey next-monkey :worry post-inspection}))

(defn sim-item-p2 [monkey worry-level]
  (let [post-inspection ((:operation monkey) worry-level)
        next-monkey ((:test monkey) post-inspection)]
    {:to-monkey next-monkey :worry post-inspection}))

(defn sim-monkey [sim-item monkey]
  (let [thrown-items (map (partial sim-item monkey) (:items monkey))
        inspected* (+ (:inspected monkey)  (count (:items monkey)))]
    {:monkey (assoc monkey :items [] :inspected inspected*)
     :thrown-items thrown-items}))

(defn sim-turn [sim-item monkeys id]
  (let [monkey (get monkeys id)
        {:keys [monkey thrown-items]} (sim-monkey sim-item monkey)
        monkeys (assoc monkeys id monkey)]
    (reduce (fn [monkeys thrown-item]
              (let [{:keys [to-monkey worry]} thrown-item
                    items (get-in monkeys [to-monkey :items])
                    items* (conj items worry)]
                (assoc-in monkeys [to-monkey :items] items*)))
            monkeys
            thrown-items)))

(defn mod-by [a] (fn [b] (mod b a)))

(defn sim-round [monkeys ids worry-mod sim-item]
  (let [monkeys (reduce (partial sim-turn sim-item) monkeys ids)
        monkeys (map-vals
                 (fn [monkey]
                   ; worry-mod is the product of all the division tests of the monkeys
                   ; since they are all prime (in my input ...) we can only track the mod
                   ; instead of the actual worry level and maintain the division test
                   (let [items (map (mod-by worry-mod) (:items monkey))]
                     (assoc monkey :items items)))
                 monkeys)]
    monkeys))

(defn part1 [[monkeys id-order worry-mod]]
  (let [monkeys (reduce (fn [monkeys _]
                          (sim-round monkeys id-order worry-mod sim-item-p1))
                        monkeys (range 20))
        inspected (map :inspected (vals monkeys))]
    (apply * (take 2 (sort > inspected)))))

(defn part2 [[monkeys id-order worry-mod]]
  (let [monkeys (reduce (fn [monkeys _]
                          (sim-round monkeys id-order worry-mod sim-item-p2))
                        monkeys (range 10000))
        inspected (map :inspected (vals monkeys))]
    (apply * (take 2 (sort > inspected)))))

(defn solve []
  (let [input (parse-input (load-input))]
    (println "Part 1:" (part1 input))
    (println "Part 2:" (part2 input))))
