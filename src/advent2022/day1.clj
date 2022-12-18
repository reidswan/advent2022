(ns advent2022.day1
  (:require [advent2022.utils :refer [group-input sum]]
            [clojure.string :refer [split-lines]]))

(defn load-input [] (slurp "resources/day1.in"))

(defn parse-input [input]
  (let [lines (split-lines input)
        groups (group-input lines)]
    (map (partial map #(Integer/parseInt %)) groups)))

(defn part1 [groups]
  (apply max (map sum groups)))

(defn part2 [groups]
  (let [cals (map sum groups)
        [m1 m2 m3] (sort > cals)]
    (+ m1 m2 m3)))

(defn solve []
  (let [input (parse-input (load-input))]
    (println "Part 1:" (part1 input))
    (println "Part 2:" (part2 input))))
