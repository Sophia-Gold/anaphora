(defproject church "0.1.0-SNAPSHOT"
  :url "https://medium.com/@sophiagoldnyc/.."
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [com.rpl/specter "1.1.0"]
                 [com.positronic-solutions/pulley.cps "0.2.2"]]
  :main ^:skip-aot church.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
