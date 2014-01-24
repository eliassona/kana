(defproject kana.crossover "NOT_DEPLOYED"
  :source-paths ["src/clj" "src/cljs"]
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [ring "1.2.0"]
                 [compojure "1.1.5"]
                 [enlive "1.1.1"]
                 [org.clojure/clojurescript "0.0-1847"]
                 ]
  :profiles {:dev {:repl-options {:init-ns kana.crossover.core}
                   :plugins [[com.cemerick/austin "0.1.1"]
                             [lein-cljsbuild "0.3.2"]]
                   :cljsbuild {:builds [{:source-paths ["src/cljs"]
                                         :compiler {:output-to "target/classes/public/app.js"
                                                    :optimizations :simple
  
  :pretty-print true

  }}]
    :crossovers [kana.crossover.core]
    :crossover-jar false
    :crossover-path "crossover-cljs"
  }}}
  :repl-options {:port 59160})