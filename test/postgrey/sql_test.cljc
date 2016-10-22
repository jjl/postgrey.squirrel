(ns postgrey.sql-test
  (:require [clojure.core.match #?(:clj :refer :cljs :refer-macros) [match]]
            [clojure.string :as str]
            [clojure.spec :as s :include-macros true]
            [clojure.spec.test :as st]
            [postgrey.sql :as sql]
   #?(:clj  [clojure.test :refer [deftest testing is]]
      :cljs [cljs.test :refer-macros [deftest testing is]]))
  (:refer-clojure :exclude [distinct group-by]))

(def emp (sql/new-state))

(def exercise-count 10)

(defn exercise [spec]
  (s/exercise spec exercise-count))

(deftest state-tests
  (let [st (sql/try-place '?foo emp)]
    (is (s/valid? ::sql/state emp))
    (is (s/valid? ::sql/state st))
    (is (= #{'?foo} (into #{} (:bindings st))))
    (is (= ::fail
           (try (sql/try-place '?foo st)
                (catch Exception e ::fail))))))

(defmethod sql/build ::mock [t v st]
  [v (inc st)])

(deftest build
  (is (= [123 2] (sql/build ::mock 123 1)))
  (is (= [[123 456] 2] (sql/map-build  ::mock [123 456] 0)))
  (is (= ["123 456" 2] (sql/join-build ::mock [123 456] 0 \space)))
  (is (= [123 2]       (sql/build-some [::mock 123] 1)))
  (is (= [[123 456] 2] (sql/map-some [[::mock 123] [::mock 456]] 0)))
  (is (= ["123 456" 2] (sql/join-some [[::mock 123] [::mock 456]] 0 \space))))  

(deftest build-makers
  (let [b (sql/make-join-build \space)
        s (sql/make-join-some \space)]
    (is (= ["123 456" 2] (b ::mock [123 456] 0)))
    (is (= ["123 456" 2] (s [[::mock 123] [::mock 456]] 0)))))

(deftest made-builders
  (is (= ["1,2,3" 3]   (sql/comma-build ::mock [1 2 3] 0)))
  (is (= ["1 2 3" 3]   (sql/space-build ::mock [1 2 3] 0)))
  (is (= ["(1,2,3)" 3] (sql/paren-comma-build ::mock [1 2 3] 0)))
  (is (= ["(1 2 3)" 3] (sql/paren-space-build ::mock [1 2 3] 0)))
  (is (= ["(1,2,3)" 3] (sql/paren-comma-some [[::mock 1] [::mock 2] [::mock 3]] 0)))
  (is (= ["(1 2 3)" 3] (sql/paren-space-some [[::mock 1] [::mock 2] [::mock 3]] 0))))

(deftest null
  (testing :null
    (doseq [[i o] (exercise ::sql/nil)]
      (is (= nil i o) "everything is nil"))
    (let [[r st] (sql/build ::sql/nil nil emp)]
      (is (= "null" r))
      (is (= st emp) "state is unmodified"))))
  
(deftest basics
  (doseq [t [::sql/int ::sql/bool ::sql/float]]
    (testing t
      (doseq [[i o] (exercise t)]
        (testing i
          (let [[r st] (sql/build t i emp)]
            (is (= i o) "conformed output should equal input")
            (is (= (str o) r) "build just stringifies")
            (is (= st emp) "output state should equal input state")))))))

(deftest string
  (doseq [[i o] (exercise ::sql/string)]
    (testing i
      (let [[r st] (sql/build ::sql/string o emp)
            r2 (str "'" (str/replace o "'" "''") "'")]
        (is (= i o)    "conformed output should equal input")
        (is (= r2 r)   "stringifies correctly")
        (is (= st emp) "output state should equal input state")))))

(deftest ident
  (doseq [[i o] (exercise ::sql/ident)]
    (testing i
      (let [[r st] (sql/build ::sql/ident o emp)]
        (is (= i o)    "conformed output should equal input")
        (is (not= "*" (name o)))
        (is (re-find #"^(?:\"(?:[^\"]|\"\")+\"\.)*\"(?:[^\"]|\"\")+\"$" r)   "stringifies correctly")
        (is (= st emp) "output state should equal input state")))))

            
(deftest ident-atom
  (doseq [[i o] (exercise ::sql/ident-atom)]
    (testing i
      (let [[r st] (sql/build ::sql/ident-atom o emp)]
        (is (= i o) "conformed output should equal input")
        (is (re-find #"^\"(?:[^\"]|\"\")+\"$" r) "stringifies correctly")
        (is (= st emp) "output state should equal input state")))))

(deftest nonbinding
  (doseq [[i o] (exercise ::sql/nonbinding)]
    (testing i
      (let [[r st] (sql/build ::sql/nonbinding o emp)]
        (is (= i o) "conformed output should equal input")
        (is (= (name o) r) "build is name")
        (is (= st emp) "output state should equal input state")))))

(deftest identy
  (doseq [[i [t o :as o2]] (exercise ::sql/identy)]
    (testing i
      (let [[r st] (sql/build ::sql/identy o2 emp)]
        (is (#{::sql/ident ::sql/nonbinding} t))
        (match o2
          [::sql/ident id]
          (is (re-find #"^(?:\"(?:[^\"]|\"\")+\"\.)*\"(?:[^\"]|\"\")+\"$" r)   "stringifies correctly")
          
          [::sql/nonbinding n]
          (is (= (name o) r)))))))

(deftest wild-kw
  (doseq [[i o] (exercise ::sql/wild-kw)]
    (testing i
      (let [[r st] (sql/build ::sql/wild-kw o emp)]
        (is (= i o)    "conformed output should equal input")
        (is (= "*" (name o)))
        (is (re-find #"^(?:\"(?:[^\"]|\"\")+\"\.)*\*$" r)   "stringifies correctly")
        (is (= st emp) "output state should equal input state")))))

(deftest literal
  (doseq [[i o] (exercise ::sql/literal)]
    (testing i
      (let [[r st] (sql/build ::sql/literal o emp)]
        (is (= (:literal o) r) "output should be the literal value")
        (is (= st emp) "output state should equal input state")))))

(deftest free
  (doseq [[i o] (exercise ::sql/free)]
    (testing i
      (let [{:keys [exprs]} o
            [es end-st] (reduce (fn [[acc st] e]
                                  (let [[r st] (sql/build-some e st)]
                                    [(conj acc r) st]))
                                [[] emp] exprs)
            es2 (str "(" (str/join " " es) ")")
            [r st] (sql/build ::sql/free o emp)]
        (is (= es2 r) "stringifies correctly")
        (is (= end-st st) "output state should contain the appropriate placeholders")))))
    
(deftest funcall)
(deftest alias)
(deftest expr)
(deftest expr+)
(deftest limit)
(deftest offset)
(deftest where)
(deftest having)
(deftest select)
(deftest distinct)
(deftest group-by)
(deftest order)
(deftest for*)
(deftest set-op)
(deftest with)
(deftest sample)
(deftest from)
(deftest window)
(deftest select-query)
(deftest table-query)
(deftest values-query)
(deftest with-query)
