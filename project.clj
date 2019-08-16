(defproject morph "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [quil "3.0.0"]
                 [uncomplicate/neanderthal "0.25.6"]]
  :main ^:skip-aot morph.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
