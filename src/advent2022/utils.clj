(ns advent2022.utils
  (:require [clojure.pprint :refer [pprint]]))

(def sum (partial apply +))

(defn trace
  ([x] (pprint x) x)
  ([msg x] (print msg) (pprint x) x))

(defn map-vals [f m]
  (into {} (map (fn [e] [(key e) (f (val e))]) m)))
