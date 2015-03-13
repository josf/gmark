(defproject gmark "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2371" :scope "provided"]]
  :plugins [[cider/cider-nrepl "0.9.0-SNAPSHOT"]
            [lein-cljsbuild "1.0.3"]]

  :test-paths ["test" "target/classes"]
  :cljsbuild {:builds
              {:app
               {:source-paths ["src"]
                :compiler {:output-to "resources/public/js/gmark.js"
                           :output-dir "resources/public/js/out"
                           :optimizations :whitespace}}}}

  :profiles {:uberjar {:hooks [leiningen.cljsbuild]
                       :cljsbuild
                       {:builds {:app
                                 {:source-paths ["target/classes"]
                                  :compiler
                                  {:optimizations :none
                                   :pretty-print false}}}}}
             :dev {:plugins [[com.keminglabs/cljx "0.5.0"]]}}

  :cljx {:builds [{:source-paths ["src/cljx"]
                   :output-path "target/classes"
                   :rules :clj}
                  {:source-paths ["src/cljx"]
                   :output-path "target/classes"
                   :rules :cljs}]}
  :prep-tasks [["cljx" "once"]])
