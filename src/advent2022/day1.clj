(ns advent2022.day1
  (:require [clojure.string :refer [split-lines]]
            [advent2022.utils :refer [sum]]))

(defn load-input [] (slurp "resources/day1.in"))

(defn group-input
  "Groups lines of input together that are separated by an empty string"
  ([input]
   (group-input input []))
  ([input groups]
   (let [[grp rem] (split-with (partial not= "") input)
         groups* (conj groups grp)]
     (if (<= (count rem) 1) ; only the "" remains in rem
       groups*
       (group-input (rest rem) groups*)))))

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
