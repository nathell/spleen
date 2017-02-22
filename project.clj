(defproject spleen "0.1.0"
  :description "A starting point for re-frame based projects."
  :dependencies [[org.clojure/clojure "1.9.0-alpha14"]
                 [re-frame "0.9.1"]
                 [cljs-ajax "0.5.8"]
                 [org.clojure/clojurescript "1.9.456" :scope "provided"]
                 [reagent "0.6.0"]]
  :plugins [[lein-cljsbuild "1.1.5"]
            [lein-figwheel "0.5.9"]]
  :clean-targets ^{:protect false}
    [:target-path
     [:cljsbuild :builds :app :compiler :output-dir]
     [:cljsbuild :builds :app :compiler :output-to]]
  :source-paths ["src/clj"]
  :resource-paths ["resources" "target/cljsbuild" "target/resources"]
  :cljsbuild {:builds {:app {:source-paths ["src/cljs"]
                             :compiler {:output-to "target/cljsbuild/public/js/app.js"
                                        :output-dir "target/cljsbuild/public/js/out"
                                        :main "spleen.core"
                                        :asset-path "js/out"
                                        :optimizations :none
                                        :pretty-print true}}}}
  :profiles {:dev {:cljsbuild {:builds {:app {:figwheel true}}}}
             :uberjar {:cljsbuild {:jar true
                                   :builds {:app
                                            {:compiler
                                             {:optimizations :advanced
                                              :pretty-print false}}}}}})
