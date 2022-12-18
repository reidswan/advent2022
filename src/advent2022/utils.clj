(ns advent2022.utils
  (:require [clojure.pprint :refer [pprint]]))

(def sum (partial apply +))

(defn trace [f & args]
  (let [res (apply f args)]
    (pprint res)
    res))

(defn map-vals [f m]
  (into {} (map (fn [e] [(key e) (f (val e))]) m)))

(defn group-input
  "Groups lines of input together that are separated by an empty string"
  ([input]
   (group-input input []))
  ([input groups]
   (let [[grp rem] (split-with (partial not= "") input)
         groups* (conj groups grp)]
     (if (<= (count rem) 1) ; only the "" remains in rem
       groups*
       (group-input (rest rem) groups*)))))

(defn digit? [a] (Character/isDigit a))
