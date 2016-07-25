(set-env!
  :project 'irresponsible/postgrey.squirrel
  :version "0.1.0"
  :resource-paths #{"src"}
  :source-paths #{"src"}
  :dependencies '[[org.clojure/clojure    "1.9.0-alpha10" :scope "provided"]
                  [org.clojure/core.match "0.3.0-alpha4"]
                  [org.flatland/ordered   "1.5.3"]
                  [com.rpl/specter        "0.11.2"]
                  [irresponsible/utrecht  "0.2.0" :scope "test"]
                  [adzerk/boot-test       "1.1.1" :scope "test"]])

(require '[adzerk.boot-test :as t])
         

(task-options!
  pom {:project (get-env :project)
       :version (get-env :version)
       :description "Next-generation SQL generation for postgresql"
       :url "https://github.com/irresponsible/postgreysquirrel"
       :scm {:url "https://github.com/irresponsible/postgreysquirrel.git"}
       :license {"MIT" "https://en.wikipedia.org/MIT_License"}}
  target  {:dir #{"target"}})

(deftask testing []
  (set-env! :source-paths   #(conj % "test")
            :resource-paths #(conj % "test"))
  identity)

(deftask test []
  (comp (target) (testing) (speak) (t/test)))

(deftask autotest []
  (comp (target) (watch) (test)))

(deftask make-jar []
  (comp (target) (pom) (jar)))

