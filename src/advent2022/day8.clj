(ns advent2022.day8
  (:require [clojure.string :refer [split-lines]]
            [clojure.set :refer [union]]))

(def max-possible-height 9)

(defn load-input [] (slurp "resources/day8.in"))

(defn get-coord [grid x y]
  (get (get grid y) x))

(defn parse-input [input]
  (let [lines (split-lines input)]
    (into [] (map
              #(into [] (map (fn [c] (- (int c) (int \0))) %))
              lines))))

(defn in-range [grid x y]
  (and (>= y 0) (< y (count grid))
       (>= x 0) (< x (count (get grid y)))))

(defn row-visible [grid x y next-elem visible tallest]
  (cond
    (>= tallest max-possible-height) visible
    (not (in-range grid x y))        visible
    :else (let [curr (get-coord grid x y)
                [x* y*] (next-elem x y)]
            (if (> curr tallest)
              (recur grid x* y* next-elem (conj visible [x y]) curr)
              (recur grid x* y* next-elem visible tallest)))))

(defn visible-from-dir
  [grid x y visible next-row next-elem]
  (if (not (in-range grid x y))
    visible
    (let [visible* (union visible
                          (row-visible grid x y next-elem #{} -1))
          [x* y*] (next-row x y)]
      (recur grid x* y* visible* next-row next-elem))))

(defn move-left [x y] [(dec' x) y])
(defn move-right [x y] [(inc' x) y])
(defn move-down [x y] [x (inc' y)])
(defn move-up [x y] [x (dec' y)])

(defn row-down [init-x _ y] [init-x (inc' y)])
(defn col-right [init-y x _] [(inc' x) init-y])

(defn visible-from-left [grid]
  (visible-from-dir grid 0 0 #{}
                    (partial row-down 0)
                    move-right))

(defn visible-from-top [grid]
  (visible-from-dir grid 0 0 #{}
                    (partial col-right 0)
                    move-down))

(defn visible-from-right [grid]
  (let [init-x (dec' (count (get grid 0)))]
    (visible-from-dir grid init-x 0 #{}
                      (partial row-down init-x)
                      move-left)))

(defn visible-from-bottom [grid]
  (let [init-y (dec' (count grid))]
    (visible-from-dir grid 0 init-y #{}
                      (partial col-right init-y)
                      move-up)))

(defn count-until->= [grid x y next-elem max-height cnt]
  (if (not (in-range grid x y))
    cnt
    (let [[x* y*] (next-elem x y)
          hgt (get-coord grid x y)]
      (if (>= hgt max-height)
        (inc' cnt)
        (recur grid x* y* next-elem max-height (inc' cnt))))))

(defn can-see-in-dir-row [grid x y next-elem known]
  (if (not (in-range grid x y))
    known
    (let [[x* y*] (next-elem x y)]
      (if (get known [x y])
        (recur grid x* y* next-elem known)
        (let [can-see (count-until->= grid x* y* next-elem (get-coord grid x y) 0)]
          (recur grid x* y* next-elem (assoc known [x y] can-see)))))))

(defn can-see-in-dir-grid [grid x y next-row next-elem known]
  (if (not (in-range grid x y))
    known
    (let [[x* y*] (next-row x y)
          known* (can-see-in-dir-row grid x y next-elem known)]
      (recur grid x* y* next-row next-elem known*))))

(defn compute-left-sight [grid]
  (let [init-x (dec' (count (get grid 0)))]
    (can-see-in-dir-grid grid init-x 0 (partial row-down init-x) move-left {})))

(defn compute-right-sight [grid]
  (can-see-in-dir-grid grid 0 0 (partial row-down 0) move-right {}))

(defn compute-up-sight [grid]
  (let [init-y (dec' (count grid))]
    (can-see-in-dir-grid grid 0 init-y (partial col-right init-y) move-up {})))

(defn compute-down-sight [grid]
  (can-see-in-dir-grid grid 0 0 (partial col-right 0) move-down {}))

(defn max-scenic-score [grid]
  (let [left (compute-left-sight grid)
        right (compute-right-sight grid)
        up (compute-up-sight grid)
        down (compute-down-sight grid)
        min-x 0
        max-x (count (get grid 0))
        min-y 0
        max-y (count grid)]
    (apply
     max
     (for [x (range min-x max-x)
           y (range min-y max-y)]
       (* (get left [x y])
          (get right [x y])
          (get up [x y])
          (get down [x y]))))))

(defn part1 [input]
  (count (union (visible-from-left input)
                (visible-from-right input)
                (visible-from-top input)
                (visible-from-bottom input))))

(defn solve []
  (let [input (parse-input (load-input))]
    (println "Part 1:" (part1 input))
    (println "Part 2:" (max-scenic-score input))))