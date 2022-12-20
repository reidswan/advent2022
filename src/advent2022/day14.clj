(ns advent2022.day14
  (:require [clojure.core.match :refer [match]]
            [clojure.string :refer [split split-lines trim]]))

(def coord-re #"(\d+)\s*,\s*(\d+)")

(def sand-origin [500 0])

(defn load-input [] (slurp "resources/day14.in"))

(defn parse-coord [coord]
  (let [[_ x y] (re-matches coord-re coord)]
    {:x (Integer/parseInt x) :y (Integer/parseInt y)}))

(defn parse-line [line]
  (let [coords (map trim (filter not-empty (split line #"\s*->\s*")))]
    (into [] (map parse-coord coords))))

(defn populate-horz [map x from-y to-y]
  (if (> from-y to-y)
    (recur map x to-y from-y)
    (reduce (fn [map y] (conj map [x y])) map (range from-y (inc to-y)))))

(defn populate-vert [map y from-x to-x]
  (if (> from-x to-x)
    (recur map y to-x from-x)
    (reduce (fn [map x] (conj map [x y])) map (range from-x (inc to-x)))))

(defn populate-from-line-segment
  ([map coords]
   (if (<= (count coords) 1)
     map
     (populate-from-line-segment map (first coords) (rest coords))))
  ([map from-coord coords]
   (if (empty? coords)
     map
     (let [{to-x :x to-y :y} (first coords)
           {from-x :x from-y :y} from-coord]
       (cond
         (= to-x from-x) (recur
                          (populate-horz map from-x from-y to-y)
                          (first coords)
                          (rest coords))
         (= to-y from-y) (recur
                          (populate-vert map from-y from-x to-x)
                          (first coords)
                          (rest coords))
         :else (throw (AssertionError. "not a horizontal or vertical line")))))))

(defn populate [line-segments]
  (reduce populate-from-line-segment #{} line-segments))

(defn parse-input [input]
  (let [input-lines (split-lines input)
        lines (map parse-line input-lines)
        filled (populate lines)
        lowest-rock (apply max (map second filled))]
    {:filled filled :lowest lowest-rock}))

(defn get-move-p1 [{filled :filled lowest :lowest} [x y]]
  (cond
    (>= y lowest) :void
    ; directly down
    (not (contains? filled [x (inc' y)]))        [x (inc' y)]
    ; down-left
    (not (contains? filled [(dec' x) (inc' y)])) [(dec' x) (inc' y)]
    ; down-right
    (not (contains? filled [(inc' x) (inc' y)])) [(inc' x) (inc' y)]
    ; no move
    :else nil))

(defn get-move-p2 [{filled :filled lowest :lowest} [x y]]
  (let [floor (+ lowest 2)]
    (cond
      ; came to rest on the floor
      (>= (inc' y) floor) nil
      ; directly down
      (not (contains? filled [x (inc' y)]))        [x (inc' y)]
      ; down-left
      (not (contains? filled [(dec' x) (inc' y)])) [(dec' x) (inc' y)]
      ; down-right
      (not (contains? filled [(inc' x) (inc' y)])) [(inc' x) (inc' y)]
      ; no move
      :else nil)))

(defn sim-sand
  ([state sand-pos get-move] (sim-sand state sand-pos nil get-move))
  ([{filled :filled :as state} [x y] prev-sand-pos get-move]
   (let [move (get-move state [x y])]
     (cond
         ; sand fell into the void!
       (= :void move) [state :void]
         ; sand come to rest
       (nil? move) [(assoc state :filled (conj filled [x y])) prev-sand-pos]
       :else (recur state move [x y] get-move)))))

(defn simulate
  ([state get-move] (simulate state sand-origin get-move))
  ([state sand-start get-move]
   (if (contains? (:filled state) sand-origin)
     ;; no more sand can flow in
     state
     (let [[state* sand-start*] (sim-sand state sand-start get-move)]
       (match sand-start*
         ;; sand has fallen into the void!
         :void state*
         ;; the sand immediately settled; restart from origin
         nil (recur state* sand-origin get-move)
         ;; continue from just before where the previous sand settled at
         :else (recur state* sand-start* get-move))))))

(defn part-1 [init-state]
  (let [final-state (simulate init-state get-move-p1)]
    [(- (count (:filled final-state)) (count (:filled init-state))) final-state]))

(defn part-2 [init-state rock-fill-count]
  (let [final-state (simulate init-state get-move-p2)]
    (- (count (:filled final-state)) rock-fill-count)))

(defn solve []
  (let [input (parse-input (load-input))
        [p1-soln init-p2-state] (part-1 input)]
    (println "Part 1:" p1-soln)
    (println "Part 2:" (part-2 init-p2-state (count (:filled input))))))
