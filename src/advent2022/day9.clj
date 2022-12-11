(ns advent2022.day9
  (:require [clojure.string :refer [split-lines trim split]]
            [clojure.core.match :refer [match]]))

(defn load-input [] (slurp "resources/day9.in"))

(defn command [dir amt]
  {:dir dir :amount amt})

(defn direction [dir]
  (match dir
    "R" :right
    "L" :left
    "U" :up
    "D" :down))

(defn parse-line [line]
  (let [[dir-str amt-str] (split line #" ")
        dir (direction dir-str)
        amt (Integer/parseInt (trim amt-str))]
    (command dir amt)))

(defn parse-input [input]
  (let [lines (split-lines input)]
    (map parse-line lines)))

(defn move-head [pos dir]
  (let [[x y] pos]
    (match dir
      :right [(inc' x)        y]
      :left  [(dec' x)        y]
      :up    [x        (inc' y)]
      :down  [x        (dec' y)])))

(defn touching? [[head-x head-y] [tail-x tail-y]]
  (and (<= (abs (- head-x tail-x)) 1)
       (<= (abs (- head-y tail-y)) 1)))

(defn move-closer [to-coord coord]
  (cond (> to-coord coord) (inc' coord)
        (< to-coord coord) (dec' coord)
        :else              coord))

(defn move-to-leader [[leader-x leader-y :as leader] [follower-x follower-y :as follower]]
  (if (touching? leader follower)
    follower
    (let [foll-x* (move-closer leader-x follower-x)
          foll-y* (move-closer leader-y follower-y)]
      [foll-x* foll-y*])))

(defn propagate-move
  ([head tail] (propagate-move head tail []))
  ([head tail done]
   (if (empty? tail)
     (conj done head)
     (let [follower (first tail)
           foll* (move-to-leader head follower)]
       (recur foll* (rest tail) (conj done head))))))

(defn move [knot-chain visited dir amt]
  (if (<= amt 0)
    [knot-chain visited]
    (let [[head & tail] knot-chain
          head-pos* (move-head head dir)
          chain* (propagate-move head-pos* tail)
          visited*  (conj visited (last chain*))]
      (recur chain* visited* dir (dec' amt)))))

(defn run-commands
  ([commands knot-count] (run-commands commands (into [] (repeat knot-count [0 0])) #{}))
  ([commands knot-chain visited]
   (if (empty? commands)
     visited
     (let [{dir :dir amount :amount} (first commands)
           [knot-chain* v*] (move knot-chain visited dir amount)]
       (recur (rest commands) knot-chain* v*)))))

(defn part-1 [input]
  (count (run-commands input 2)))

(defn part-2 [input]
  (count (run-commands input 10)))

(defn solve []
  (let [input (parse-input (load-input))]
    (println "Part 1:" (part-1 input))
    (println "Part 2:" (part-2 input))))
