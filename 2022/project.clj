(defproject year_2022 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/core.async "1.6.673"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [org.clojure/math.numeric-tower "0.0.5"]
                 [org.clojure/data.priority-map "1.1.0"]
                 [quil "4.0.0-SNAPSHOT"]]
  :resource-paths ["resources/rebl-0.9.245.jar"]
  :repl-options {:init-ns year-2022.core}
  :main year_2022.day12/-main
  :jvm-opts ["-Xss10m" ]
  :plugins [[lein-cljfmt "0.5.6"]]
  :user {
         :plugins [[cider/cider-nrepl "0.9.0"]]}

  )
;; (require '[cognitect.rebl :as rebl])
;; (rebl/ui)


;; clj -A:rebl:nREPL

;; clojure -M:lib/cider-nrepl:inspect/rebl:middleware/nrebl
