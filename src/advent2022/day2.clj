(ns advent2022.day2
  (:require [clojure.string :refer [split-lines]]
            [advent2022.utils :refer [sum]]))

(def line-re #"(.) (.)") ; boobs

(defn load-input [] (slurp "resources/day2.in"))

(defn parse-shape [shape]
  (case shape
    "A" :rock
    "B" :paper
    "C" :scissors
    "X" :rock
    "Y" :paper
    "Z" :scissors))

(defn parse-outcome [outcome]
  (case outcome
    "X" :loss
    "Y" :draw
    "Z" :win))

(defn parse-line [line]
  (let [match (re-matches line-re line)]
    (rest match)))

(defn parse-input [input]
  (let [lines (split-lines input)]
    (map parse-line lines)))

(defn make-maps [map-maker]
  {:rock (map-maker :paper :rock :scissors)
   :paper (map-maker :scissors :paper :rock)
   :scissors (map-maker :rock :scissors :paper)})

(defn outcome-map [win draw loss] {loss :loss draw :draw win :win})

(def play-outcomes (make-maps outcome-map))

(defn play-map [win draw loss] {:loss loss :draw draw  :win win})

(def outcome-plays (make-maps play-map))

(defn shape-score [shape]
  (case shape
    :rock 1
    :paper 2
    :scissors 3
    0))

(defn result-score [result]
  (case result
    :loss 0
    :draw 3
    :win 6
    0))

(defn score [shape outcome]
  (+ (shape-score shape) (result-score outcome)))

(defn part1-match [match]
  (let [[opp play] (map parse-shape match)
        outcome (get-in play-outcomes [opp play])]
    (score play outcome)))

(defn part1 [matches]
  (sum (map part1-match matches)))

(defn part2-match [match]
  (let [[opp-c res-c] match
        opp-play (parse-shape opp-c)
        result (parse-outcome res-c)
        my-play (get-in outcome-plays [opp-play result])]
    (score my-play result)))

(defn part2 [matches]
  (sum (map part2-match matches)))

(defn solve []
  (let [input (parse-input (load-input))]
    (println "Part 1:" (part1 input))
    (println "Part 2:" (part2 input))))
