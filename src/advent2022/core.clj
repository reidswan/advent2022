(ns advent2022.core
  (:gen-class)
  (:require [advent2022.day1  :as day1]
            [advent2022.day2  :as day2]
            [advent2022.day3  :as day3]
            [advent2022.day4  :as day4]
            [advent2022.day5  :as day5]
            [advent2022.day6  :as day6]
            [advent2022.day7  :as day7]
            [advent2022.day8  :as day8]
            [advent2022.day9  :as day9]
            [advent2022.day10 :as day10]
            [advent2022.day11 :as day11]
            [advent2022.day12 :as day12]
            [advent2022.day13 :as day13]
            [advent2022.day14 :as day14]
            [advent2022.day15 :as day15]))

(def solns
  {"1"  day1/solve
   "2"  day2/solve
   "3"  day3/solve
   "4"  day4/solve
   "5"  day5/solve
   "6"  day6/solve
   "7"  day7/solve
   "8"  day8/solve
   "9"  day9/solve
   "10" day10/solve
   "11" day11/solve
   "12" day12/solve
   "13" day13/solve
   "14" day14/solve
   "15" day15/solve})

(defn run
  "Attempt to run the solution for the given day"
  [x]
  (let [solver (solns x)]
    (if (nil? solver)
      (println "no solver for day" x)
      (do
        (println "day" x)
        (solver)))))

(defn -main
  "Advent 2022 main function; call `lein run <...>` to run the given days"
  [& args]
  (doseq [arg args]
    (run arg)))
