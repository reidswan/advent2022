(defproject advent2022 "0.1.0-SNAPSHOT"
  :description "Advent of code 2022 solutions"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.0"] 
                 [org.clojure/core.match "1.0.0"]]
  :main ^:skip-aot advent2022.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
