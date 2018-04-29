(defproject anaphora "0.1.0-SNAPSHOT"
  :url "https://medium.com/@sophiagoldnyc/anaphora"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/data.avl "0.0.17"]
                 [org.clojure/data.int-map "0.2.4"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [com.rpl/specter "1.1.0"]
                 [fipp "0.6.12"]
                 [com.positronic-solutions/pulley.cps "0.2.2"]
                 [org.clojure/tools.analyzer "0.6.9"]
                 [org.clojure/tools.analyzer.jvm "0.7.2"]
                 [bronsa/tools.decompiler "0.1.0-alpha1"]] 
  :plugins [[lein-nodisassemble "0.1.3"]] 
  :main ^:skip-aot anaphora
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
