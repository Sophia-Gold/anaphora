(defproject anaphora "0.1.0-SNAPSHOT"
  :url "https://medium.com/@sophiagoldnyc/anaphora"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[local/clojure "1.9.999999"]
                 [org.clojure/spec.alpha "0.1.143"]
                 [org.clojure/core.specs.alpha "0.1.24"]
                 [expound "0.7.0"]
                 [org.clojure/data.avl "0.0.17"]
                 [org.clojure/data.int-map "0.2.4"]
                 [org.clojure/math.combinatorics "0.1.4"] 
                 [org.clojure/tools.analyzer "0.6.9"]
                 [org.clojure/tools.analyzer.jvm "0.7.2"]
                 [com.rpl/specter "1.1.0"]
                 [fipp "0.6.12"]] 
  :repositories {"local" ~(str (.toURI (java.io.File. "maven_repository")))}
  :main ^:skip-aot anaphora
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
