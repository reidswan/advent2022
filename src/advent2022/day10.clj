(ns advent2022.day10
  (:require [advent2022.utils :refer [trace]]
            [clojure.core.match :refer [match]]
            [clojure.string :refer [join split-lines trim]]))

(defn load-input [] (slurp "resources/day10.in"))

(def addx-re #"addx (-?\d+)\s*")

(defn parse-command [line]
  (if (= (trim line) "noop")
    [{:type :noop}]
    (if-let [[_ num-str] (re-matches addx-re line)]
      ; pad with a noop! so we don't have to count cycles!
      [{:type :noop} {:type :addx :amount (Integer/parseInt num-str)}]
      (throw (AssertionError. (str "expected noop or addx [num] but got " line))))))

(defn parse-input [input]
  (let [lines (split-lines input)]
    (mapcat parse-command lines)))

(defn enact [val command]
  (match (:type command)
    :noop val
    :addx (+ val (:amount command))))

(defn sim-commands
  [commands init-reg] (reduce enact init-reg commands))

(defn overlaps? [sprite pixel-horz]
  (<= (abs (- sprite pixel-horz)) 1))

(defn cycle-horz-position [cycle]
  (mod cycle 40))

(defn register-values [commands]
  (second
   (reduce
    (fn [[register registers] command]
      (let [reg* (enact register command)]
        [reg* (conj registers reg*)]))
    [1 []]
    commands)))

(defn draw [values]
  (apply str
         (map-indexed
          (fn [cycle-n sprite-horz]
            (let [index (cycle-horz-position cycle-n)]
              (if (overlaps? sprite-horz (inc index))
                \#
                \space)))
          values)))

(defn part-1 [input]
  (let [[first-set rest] (split-at 19 input) ; split-at 19 since the register value at the end of 19 == reg val during 20
        rest-sets (take 5 (partition 40 rest))
        command-sets (into [first-set] rest-sets)]
    (first (reduce
            (fn [[tot-sig-str reg-val cycle-count] command-set]
              (let [reg-val* (sim-commands command-set reg-val)
                    cycle-count* (+ cycle-count (count command-set))
                    sig-str (* reg-val* cycle-count*)
                    tot-sig-str* (+ tot-sig-str sig-str)]
                [tot-sig-str* reg-val* cycle-count*])) [0 1 1] command-sets))))

(defn part-2 [input]
  (let [values (register-values input)
        lines (partition 40 values)
        pixels (map draw lines)]
    (str "\n" (join "\n" pixels))))

(defn solve []
  (let [input (parse-input (load-input))]
    (println "Part 1:" (part-1 input))
    (println "Part 2:" (part-2 input))))
