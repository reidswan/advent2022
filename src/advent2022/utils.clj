(ns advent2022.utils
  (:require [clojure.pprint :refer [pprint]]))

(def sum (partial apply +))

(defn trace
  ([x] (pprint x) x)
  ([msg x] (pprint msg x) x))
