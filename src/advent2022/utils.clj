(ns advent2022.utils)

(def sum (partial apply +))

(defn trace
  ([x] (println x) x)
  ([msg x] (println msg x) x))
