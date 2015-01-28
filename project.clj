(defproject com.lemondronor/turboshrimp-tracker "0.1.0-SNAPSHOT"
  :description "AR.Drone controller with video tracking."
  :url "http://example.com/FIXME"
  :license {:name "MIT License"}
  :dependencies [[com.lemondronor/turboshrimp "0.3.7"]
                 [com.lemondronor/turboshrimp-xuggler "0.0.2"]
                 [nu.pattern/opencv "2.4.9-7"]
                 [org.clojure/clojure "1.6.0"]
                 [org.clojure/core.incubator "0.1.3"]
                 [seesaw "1.4.4"]]
  :main ^:skip-aot com.lemondronor/turboshrimp-tracker.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
