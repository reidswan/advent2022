(ns advent2022.day3
  (:require [clojure.set :as set]
            [clojure.string :refer [split-lines]]
            [advent2022.utils :refer [sum]]))

(defn load-input [] (slurp "resources/day3.in"))

(defn split-line [line]
  (let [[comp1 comp2] (split-at (/ (count line) 2) line)]
    [(into #{} comp1) (into #{} comp2)]))

(defn parse-input [input]
  (let [lines (split-lines input)]
    (map split-line lines)))

(def upper-ord (int \A))
(def lower-ord (int \a))

(defn priority
  "Map an item identifier to its priority"
  [ch]
  (let [ord (int ch)]
    (if (>= ord lower-ord)
    ; a lower-case letter; a=1->z=26
      (+ 1 (- ord lower-ord))
    ; an upper-case letter; A=27->z=52
      (+ 27 (- ord upper-ord)))))

(defn find-overlap [& c]
  (first (apply set/intersection c)))

(defn part1 [rucksacks]
  (let [overlaps (map (partial apply find-overlap) rucksacks)]
    (sum (map priority overlaps))))

(defn recombine-rucksack [rucksack] (apply set/union rucksack))

(defn part2 [rucksacks]
  (let [trios (partition 3 rucksacks)
        recombined-trios (map #(map recombine-rucksack %) trios)
        badges (map (partial apply find-overlap) recombined-trios)
        priorities (map priority badges)]
    (sum priorities)))

(defn solve []
  (let [input (parse-input (load-input))]
    (println "Part 1:" (part1 input))
    (println "Part 2:" (part2 input))))
