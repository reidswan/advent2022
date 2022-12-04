(ns advent2022.day4
  (:require [clojure.string :refer [split-lines]]))

(defn crange [from to] {:from from :to to})

(def line-re #"(\d+)\-(\d+),\s*(\d+)-(\d+)")

(defn load-input [] (slurp "resources/day4.in"))

(defn parse-line [line]
  (let [matches (rest (re-matches line-re line))
        [from1 to1 from2 to2] (map #(Integer/parseInt %) matches)]
    [(crange from1 to1) (crange from2 to2)]))

(defn parse-input [input]
  (let [lines (split-lines input)
        ranges (map parse-line lines)]
    ranges))

(defn sign [x]
  (if (= x 0)
    :zero
    (if (> x 0)
      :pos
      :neg)))

(defn crange-contains?
  "Determine if either of the two cranges completely contains the other"
  [[range1 range2]]
  (let [sfrom (sign (- (:from range1) (:from range2)))
        sto (sign (- (:to range1) (:to range2)))]
    (or
     ; if the ranges start or end at the same point, one certainly contains the other
     (= sfrom :zero)
     (= sto :zero)
     ; otherwise, if from1 > from2 and to1 < to2, or from1 < from2 and to1 > to2,
     ; one is contained by the other -- i.e. the sign of their difference is not the same
     (not= sfrom sto))))

(defn no-overlap
  "Determine if the two cranges are completely disjoint"
  [[range1 range2]]
  (or
   (> (:from range1) (:to range2))
   (> (:from range2) (:to range1))))

(defn part1 [range-pairs]
  (count (filter crange-contains? range-pairs)))

(defn part2 [range-pairs]
  (count (filter (comp not no-overlap) range-pairs)))

(defn solve []
  (let [input (parse-input (load-input))]
    (println "Part 1: " (part1 input))
    (println "Part 2: " (part2 input))))
