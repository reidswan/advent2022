(ns advent2022.day12
  (:require [advent2022.utils :refer [trace]]
            [clojure.core.match :refer [match]]
            [clojure.string :refer [blank? index-of split-lines]]))

(defn load-input [] (slurp "resources/day12.in"))

(defn parse-height [c]
  (when (not (char? c)) (throw (AssertionError. (str c " is not a char"))))
  (match c
    \S 0
    \E (- (int \z) (int \a))
    :else (- (int c) (int \a))))

(defn find-coord
  ([lines ch] (find-coord lines ch 0))
  ([lines ch row-index]
   (if (empty? lines)
     nil
     (if-let [col-index (index-of (first lines) ch)]
       [row-index col-index]
       (recur (rest lines) ch (inc row-index))))))

(defn parse-input [input]
  (let
   [lines (filter (complement blank?) (split-lines input))
    start-coord (find-coord lines \S)
    end-coord (find-coord lines \E)]
    [(into [] (map #(into [] (map parse-height %)) lines)) start-coord end-coord]))

(defn can-move [grid from [to-row to-col :as to]]
  (cond
    (or (< to-row 0) (< to-col 0)) false
    (>= to-row (count grid)) false
    (>= to-col (count (get grid to-row))) false
    :else (let [from-height (get-in grid from)
                to-height (get-in grid to)]
            ; reversed vs the AoC spec; we're finding paths 'from' E 'to' S instead
            (<= (- from-height to-height) 1))))

(def adjacent-deltas [[1 0] [-1 0] [0 1] [0 -1]])

(defn adjacent [a b]
  (for [[a' b'] adjacent-deltas]
    [(+ a a') (+ b b')]))

(defn next-moves [grid [from-row from-col :as from]]
  (let [poss-next-moves (adjacent from-row from-col)]
    (filter (partial can-move grid from) poss-next-moves)))

(defn next-node [dists unvisited]
  (if (empty? unvisited) nil
      (apply (partial min-key #(get dists % ##Inf)) unvisited)))

(defn all-coords ([grid] (all-coords grid (constantly true)))
  ([grid pred]
   (into #{} (for [i (range (count grid))
                   j (range (count (get grid 0)))
                   :when (pred grid [i j])]
               [i j]))))

(defn shortest-paths
  ([grid source] (shortest-paths grid {source 0} (all-coords grid)))
  ([grid dists unvisited]
   (if (empty? unvisited) dists
       (let [curr (next-node dists unvisited)
             unvisited* (disj unvisited curr)
             neighbors (next-moves grid curr)
             curr-dist (get dists curr ##Inf)
             dists* (reduce (fn [dists coord]
                              (let [known-dist (get dists coord ##Inf)
                                    min-dist (min (inc curr-dist) known-dist)]
                                (if (not= ##Inf min-dist)
                                  (assoc dists coord min-dist)
                                  dists))) dists neighbors)]
         (recur grid dists* unvisited*)))))

(defn shortest-path-from-0-elevation [paths grid]
  (let [to-coords (all-coords grid (fn [grid coord] (= (get-in grid coord) 0)))
        path-lengths (filter (complement nil?) (map #(get paths %) to-coords))]
    (apply min path-lengths)))

(defn solve []
  (let [[grid start end] (parse-input (load-input))
        paths (shortest-paths grid end)]
    (println "Part 1:" (get paths start))
    (println "Part 2:" (shortest-path-from-0-elevation paths grid))))
