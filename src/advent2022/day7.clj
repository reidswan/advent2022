(ns advent2022.day7
  (:require [clojure.string :refer [split-lines trim]]
            [advent2022.utils :refer [sum]]
            [clojure.core.match :refer [match]]))

(def cd-re #"\$ cd (.+)\s*")
(def ls-re #"\$ ls\s*")
(def dir-re #"dir (.*)\s*")
(def file-re #"(\d+)\s(.+)\s*")
(def root "/")
(def up "..")

(def max-size 100000)
(def total-disk-space 70000000)
(def required-disk-space 30000000)

(defn trace
  ([x] (println x) x)
  ([msg x] (println msg x) x))

(defn is-command [line]
  (= (first (trim line)) \$))

(defn empty-dir [dir parent-name]
  {:type :dir-tree :dir dir :children #{} :parent-name parent-name})


(defn path-join [par chil]
  (if (= par root) (str "/" chil)
      (str par "/" chil)))

(defn full-name [{:keys [dir parent-name]}]
  (path-join parent-name dir))

(defn exec-cd [to-directory curr-directory dirs]
  (cond
    ; `cd /` should only show up once, at start, so just init new dir-tree
    (= to-directory root) (empty-dir "" "")
    (= to-directory up)   (if (= (full-name curr-directory) root)
                            (throw (AssertionError. "trying to navigate up from root"))
                            (get dirs (:parent-name curr-directory)))
    :else              (get dirs (path-join (full-name curr-directory) to-directory))))


(defn exec-ls [curr-directory output dirs]
  (let [children (reduce (fn [children child]
                           (assoc children (path-join (full-name curr-directory) (:name child))
                                  (match (:type child)
                                    :file     child
                                    :dir-line (empty-dir (:name child) (full-name curr-directory)))))
                         {}
                         output)
        curr-directory (assoc curr-directory :children (into #{} (keys children)))
        dirs (assoc (merge dirs children) (full-name curr-directory) curr-directory)]
    dirs))

(defn load-input [] (slurp "resources/day7.in"))

(defn ls-cmd [output]
  {:type :ls :output output})

(defn cd-cmd [dir-name]
  {:type :cd :dir-name dir-name})

(defn file-entry [name size]
  {:type :file :name name :size size})

(defn directory-entry [name]
  {:type :dir-line :name name})

(defn parse-ls-entry [line]
  (if-let [[_ size name] (re-matches file-re line)]
    (file-entry name (Integer/parseInt size))
    (if-let [[_ name] (re-matches dir-re line)]
      (directory-entry name)
      (throw (AssertionError. (str "expected file or directory but got " line))))))

(defn parse-input
  ([input]
   (let [lines (split-lines input)]
     (parse-input lines [])))
  ([lines parsed-commands]
   (if (empty? lines) parsed-commands
       (let [[head & rest] lines]
         (if-let [[_ directory] (re-matches cd-re head)]
           (recur rest (conj parsed-commands (cd-cmd directory)))
           (if (re-matches ls-re head)
             (let [[fs rest] (split-with (comp not is-command) rest)
                   entries (map parse-ls-entry fs)
                   cmd (ls-cmd entries)]
               (recur rest (conj parsed-commands cmd)))
             (throw (AssertionError. (str "expected cd or ls but got " head)))))))))

(defn exec-cmd [[dirs curr-directory] cmd]
  (cond
    (= (:type cmd) :cd) (let [new-curr-dir (exec-cd (:dir-name cmd) curr-directory dirs)]
                          [dirs new-curr-dir])
    (= (:type cmd) :ls) (let [new-dirs (exec-ls curr-directory (:output cmd) dirs)]
                          [new-dirs curr-directory])))

(defn exec-commands [commands]
  (first (reduce exec-cmd [{} {}] commands)))

(defn compute-sizes
  ([fs] (compute-sizes fs root {}))
  ([fs curr-file known-sizes]
   (let [entry (get fs curr-file)
         known-size (get known-sizes curr-file)]
     (cond
       known-size known-sizes
       (= (:type entry) :file) (assoc known-sizes curr-file (:size entry))
       :else (let [children (:children entry)
                   known-sizes* (reduce #(compute-sizes fs %2 %1) known-sizes children)
                   size (sum (map #(get known-sizes* %) children))]
               (assoc known-sizes* curr-file size))))))

(defn directory-sizes [sizes directories]
  (into {} (filter #(not= (:type (get directories (key %))) :file) sizes)))

(defn part1 [sizes]
  (sum (filter #(<= % max-size) (vals sizes))))

(defn part2 [sizes]
  (let [total-used-space (get sizes root)
        current-free (- total-disk-space total-used-space)
        must-free (- required-disk-space current-free)]
    (apply min (filter #(>= % must-free) (vals sizes)))))

(defn solve []
  (let [input (parse-input (load-input))
        directories (exec-commands input)
        sizes (directory-sizes (compute-sizes directories) directories)]
    (println "Part 1:" (part1 sizes))
    (println "Part 2:" (part2 sizes))))
