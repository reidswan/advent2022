(ns advent2022.day13
  (:require [advent2022.utils :refer [digit? group-input trace]]
            [clojure.core.match :refer [match]]
            [clojure.string :refer [split-lines]]))

(defn load-input [] (slurp "resources/day13.in"))

(defn number' [a] {:type :number :value a})
(defn list' [a] {:type :list :value a})

; needed for mutual recursion between parse-list and parse-next
(declare parse-next)

(defn parse-number [stream]
  (let [[num-ch rest-stream] (split-with digit? stream)
        num-str (apply str num-ch)]
    [(number' (Integer/parseInt num-str)) rest-stream]))

(defn parse-list
  ([[head & stream]]
   (when (not= head \[)
     (throw (AssertionError. (str "expected list to start with '[' but got '" head))))
   (parse-list stream []))
  ([stream items]
   (match (first stream)
     \] [(list' items) (rest stream)]
     (:or \, \space) (recur (rest stream) items)
     :else (let [[next rest-stream] (parse-next stream)]
             (recur rest-stream (conj items next))))))

(defn parse-next [stream]
  (when (empty? stream)
    ; maybe return an eof?
    (throw (AssertionError. "nothing left to parse")))
  (match (first stream)
    \[ (parse-list stream)
    (_ :guard digit?) (parse-number stream)
    (:or \, \space) (recur (rest stream))
    :else (throw (AssertionError. (str "unexpected char '" (first stream) "'")))))

(defn parse-line [line]
  (let [[it rest] (parse-next line)]
    (if (empty? rest)
      it
      (throw (AssertionError. "expected end of input after parsing item in line")))))

(defn parse-pair [lines]
  (map parse-line lines))

(defn parse-input [input]
  (let [groups (group-input (split-lines input))]
    (map parse-pair groups)))

(defn normalize [a b]
  (cond
    (= (:type a) (:type b)) [a b]
    (= (:type a) :number) [(list' [a]) b]
    :else [a (list' [b])]))

(declare well-ordered?)

(defn well-ordered-list? [a b]
  (cond
    (and (empty? a) (empty? b)) nil
    (empty? a) true
    (empty? b) false
    :else (let [first-wo (well-ordered? (first a) (first b))]
            (if (nil? first-wo)
              (recur (rest a) (rest b))
              first-wo))))

(defn well-ordered-numbers? [a b]
  (cond
    (< a b) true
    (> a b) false
    :else nil))

(defn well-ordered? [a b]
  (let [[a b] (normalize a b)]
    (cond
      (= (:type a) :number) (well-ordered-numbers? (:value a) (:value b))
      :else (well-ordered-list? (:value a) (:value b)))))

(defn part-1 [input]
  (let [indexed-groups (map-indexed list input)
        well-ordered (filter #(apply well-ordered? (second %)) indexed-groups)]
    (apply + (map #(inc (first %)) well-ordered))))

(defn divider [a] (assoc (list' [(list' [(number' a)])]) :divider true))

(defn part-2 [input]
  (let [elms (conj (apply concat input)
                   ; divider packets
                   (divider 2)
                   (divider 6))
        sorted (sort well-ordered? elms)
        divider-indices (keep-indexed #(when (:divider %2) (inc %1)) sorted)]
    (apply * divider-indices)))

(defn solve []
  (let [input (parse-input (load-input))]
    (println "Part 1:" (part-1 input))
    (println "Part 2:" (part-2 input))))
