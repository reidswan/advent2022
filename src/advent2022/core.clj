(ns advent2022.core
  (:gen-class)
  (:require [advent2022.day1 :as day1]
            [advent2022.day2 :as day2]
            [advent2022.day3 :as day3]
            [advent2022.day4 :as day4]))

(def solns
  {"1" day1/solve
   "2" day2/solve
   "3" day3/solve
   "4" day4/solve})

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
