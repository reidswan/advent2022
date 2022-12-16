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
            [advent2022.day11 :as day11]))

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
   "11" day11/solve})

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
