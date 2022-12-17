(ns advent2022.utils
  (:require [clojure.pprint :refer [pprint]]))

(def sum (partial apply +))

(defn trace [f & args]
  (let [res (apply f args)]
    (pprint res)
    res))

(defn map-vals [f m]
  (into {} (map (fn [e] [(key e) (f (val e))]) m)))
