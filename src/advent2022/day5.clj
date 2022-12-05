(ns advent2022.day5
  (:require [clojure.string :refer [split-lines split trim]]
            [clojure.core.reducers :refer [fold]]))

(defn load-input [] (slurp "resources/day5.in"))

(def crate-row-re #"\[(.)\]\s?")

(def command-re #"move (\d+) from (\d+) to (\d+)\s*")

(defn empty-stacks [stack-ids]
  (fold (fn
          ([stacks id] (assoc stacks id '()))
          ([] {})) ; base case - empty map
        stack-ids))

(defn maybe-crate-id [cs]
  (let [match (re-matches crate-row-re cs)]
    (if match (second match) nil)))

(defn populate-stacks-from-line [stacks line stack-ids]
  (if (or (empty? line) (empty? stack-ids))
    stacks
    (let [[next-crate rest-line] (map #(apply str %) (split-at 4 line))
          [next-id & rest-stack-ids] stack-ids
          curr-stack (get stacks next-id)
          crate-id (maybe-crate-id next-crate)
          next-stacks (if crate-id
                        (assoc stacks next-id (conj curr-stack crate-id))
                        stacks)]
      (populate-stacks-from-line
       next-stacks
       rest-line
       rest-stack-ids))))

(defn populate-stacks [crate-lines stacks stack-ids]
  (fold (fn
          ([] stacks) ; base case - the supplied stacks map
          ([stacks line] (populate-stacks-from-line stacks line stack-ids)))
        crate-lines))

(defn parse-stacks [stacks-str]
  (let [stacks-str (reverse stacks-str)
        [stack-desc & crates] stacks-str
        stack-ids (split (trim stack-desc) #"\s+")
        stacks (empty-stacks stack-ids)]
    {:stacks (populate-stacks crates stacks stack-ids)
     :ids stack-ids}))

(defn parse-command [command]
  (let [[_ count from to] (re-matches command-re command)]
    {:count (Integer/parseInt count)
     :from from
     :to to}))

(defn parse-commands [commands]
  (map parse-command commands))

(defn parse-input [input]
  (let [lines (split-lines input)
        [stacks commands*] (split-with (comp not empty?) lines)
        commands (rest commands*)] ; drop the empty line from the head of commands*
    {:stacks (parse-stacks stacks)
     :commands (parse-commands commands)}))

(defn run-command-p1 [{from :from to :to count :count} stacks]
  (let [from-stack (get stacks from)
        to-stack (get stacks to)
        [moved remaining] (split-at count from-stack)
        to-stack* (concat (reverse moved) to-stack)]
    (assoc stacks
           from remaining
           to to-stack*)))

(defn run-command-p2 [{from :from to :to count :count} stacks]
  (let [from-stack (get stacks from)
        to-stack (get stacks to)
        [moved remaining] (split-at count from-stack)
        to-stack* (concat moved to-stack)]
    (assoc stacks
           from remaining
           to to-stack*)))

(defn run-commands [runner stacks commands]
  (fold (fn
          ([] stacks) ; base case - the supplied stacks map
          ([stacks command] (runner command stacks)))
        commands))

(defn get-top-layer [stacks ids]
  (apply str (map #(first (get stacks %)) ids)))

(defn part1 [{stacks :stacks ids :ids} commands]
  (let [final-stacks (run-commands run-command-p1 stacks commands)]
    (get-top-layer final-stacks ids)))

(defn part2 [{stacks :stacks ids :ids} commands]
  (let [final-stacks (run-commands run-command-p2 stacks commands)]
    (get-top-layer final-stacks ids)))

(defn solve []
  (let [{stacks :stacks
         commands :commands} (parse-input (load-input))]
    (println "Part 1:" (part1 stacks commands))
    (println "Part 2:" (part2 stacks commands))))
