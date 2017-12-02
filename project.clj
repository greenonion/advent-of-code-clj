(defproject advent-of-code "0.1.0-SNAPSHOT"
  :description "Advent of code solutions in Clojure"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-RC2"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [pandect "0.5.4"]
                 [net.mikera/core.matrix "0.47.0"]]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
