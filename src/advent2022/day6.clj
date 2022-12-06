(ns advent2022.day6)

(defn load-input [] (slurp "resources/day6.in"))

(defn find-marker
  ([marker-len chars] (find-marker marker-len chars 0))
  ([marker-len chars cnt]
   (if (empty? chars)
    -1
    (let [head (take marker-len chars)
          head-uniq-size (count (into #{} head))]
      (if (= head-uniq-size marker-len)
        (+ cnt marker-len)
        (find-marker marker-len (rest chars) (inc' cnt)))))))

(defn solve []
  (let [input (load-input)
        part1 (find-marker 4 input)
        part1-marker-start (- part1 4)
        part2-input (drop part1-marker-start input)
        part2 (+ part1-marker-start (find-marker 14 part2-input))]
    (println "Part 1:" part1)
    (println "Part 2:" part2)))
