(ns advent2022.day15
  (:require [advent2022.utils :refer [sum]]
            [clojure.set :refer [union]]
            [clojure.string :refer [split-lines]]))

(def line-re #".*x=(-?\d+), y=(-?\d+).*x=(-?\d+), y=(-?\d+).*")

(defn load-input [] (slurp "resources/day15.in"))

(defn parse-coord [x-str y-str]
  {:x (Integer/parseInt x-str)
   :y (Integer/parseInt y-str)})

(defn parse-sensor [line]
  (if-let [[_ sx sy bx by] (re-matches line-re line)]
    {:location (parse-coord sx sy)
     :beacon (parse-coord bx by)}
    (throw (AssertionError. (str "'" line "' did not match line-re")))))

(defn parse-input [input]
  (let [lines (split-lines input)]
    (map parse-sensor lines)))

(defn manhattan-distance [{:keys [location beacon]}]
  (+ (abs (- (:x location) (:x beacon)))
     (abs (- (:y location) (:y beacon)))))

(defn excluded-row-range
  "The inclusive range of cells in the given row that cannot contain beacons"
  [{:keys [location] :as sensor} row]
  (let [md (manhattan-distance sensor)
        x-delta (- md (abs (- (:y location) row)))
        from (- (:x location) x-delta)
        to (+ (:x location) x-delta)]
    {:from from :to to}))

(defn row-contains? [{:keys [from to]} point]
  (<= from point to))

(defn rows-overlap? [range-1 {:keys [from to]}]
  (or (row-contains? range-1 from)
      (row-contains? range-1 to)))

(defn range-fully-contained? [container containee]
  (and (row-contains? container (:from containee))
       (row-contains? container (:to containee))))

(defn row-union [{from1 :from to1 :to} {from2 :from to2 :to}]
  {:from (min from1 from2)
   :to (max to1 to2)})

(defn combine-rows [rows]
  (let [sorted-rows (sort (fn [r1 r2] (< (:from r1) (:from r2))) rows)]
    (reduce (fn [combined-ranges range]
              (cond
                (< (:to range) (:from range)) combined-ranges
                (rows-overlap? (peek combined-ranges) range)
                (let [rest-ranges (pop combined-ranges)
                      union (row-union (peek combined-ranges) range)]
                  (conj rest-ranges union))
                :else (conj combined-ranges range)))
            [(first sorted-rows)]
            (rest sorted-rows))))

(defn range-size [{:keys [from to]}]
  (- to from))

(defn part-1 [input]
  (let [row 2000000
        ranges (map #(excluded-row-range % row) input)
        combo-ranges (combine-rows ranges)
        range-sizes (map range-size combo-ranges)]
    (sum range-sizes)))

(defn find-first-uncovered
  ([input] (find-first-uncovered input 0))
  ([input curr-row]
   (when (> curr-row 4000000) (throw (AssertionError. "no uncovered cell in range")))
   (let [ranges (map #(excluded-row-range % curr-row) input)
         combo-ranges (combine-rows ranges)]
     (when (zero? (mod curr-row 1000)) (println curr-row))
     (if (some #(range-fully-contained? % {:from 0 :to 4000000}) combo-ranges)
       (recur input (inc curr-row))
       ; this isn't very robust so fingers crossed that it works
       {:y curr-row :x (inc (:to (first combo-ranges)))}))))

(defn part-2 [input]
  (let [{:keys [x y]} (find-first-uncovered input)]
    (+ (* 4000000 x) y)))

(defn solve []
  (let [input (parse-input (load-input))]
    (println "Part 1:" (part-1 input))
    (println "Part 2:" (part-2 input))))
