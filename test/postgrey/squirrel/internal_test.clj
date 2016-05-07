(ns postgrey.squirrel.internal-test
  (:use [clojure.test])
  (:require [postgrey.squirrel.internal :as i]
            [postgrey.squirrel.util :as u])
  (:import [java.lang Exception])
  (:refer-clojure :exclude [cast]))

(deftest expr-vec
  (is (= "( \"foo\" :: \"bar\" )"  (i/expr-vec {} [:cast :foo :bar])))
  (is (= "( \"foo\" and \"bar\" )" (i/expr-vec {} [:and :foo :bar])))
  (is (= "( \"foo\" or \"bar\" )"  (i/expr-vec {} [:or :foo :bar])))
  (is (= "( \"foo\" is null )"     (i/expr-vec {} [:null? :foo])))
  (is (= "( \"foo\" is not null )" (i/expr-vec {} [:not-null? :foo])))
  (is (= "\"now\"(  )"             (i/expr-vec {} [:now<>]))))

(deftest expr
  (is (= "true"  (i/expr {} true)))
  (is (= "false" (i/expr {} false)))
  (is (= "\"bar \"\"foo\"\" \".\"\"\"baz\""
         (i/expr {} (keyword "bar \"foo\" /\"baz"))))
  (is (= "( \"foo\" :: \"bar\" )"  (i/expr {} [:cast :foo :bar])))
  (is (= "( \"foo\" and \"bar\" )" (i/expr {} [:and :foo :bar])))
  (is (= "( \"foo\" or \"bar\" )"  (i/expr {} [:or :foo :bar])))
  (is (= "( \"foo\" is null )"     (i/expr {} [:null? :foo])))
  (is (= "( \"foo\" is not null )" (i/expr {} [:not-null? :foo])))
  (is (= "\"now\"(  )"             (i/expr {} [:now<>]))))

;; todo: lateral
(deftest from-on
  (is (nil? (i/from-on {} nil)))
  (is (= " on \"a\" "  (i/from-on {} :a)))
  (is (= " on true "  (i/from-on {} true))))

;; todo: subquery aliases
(deftest join
  (doseq [i [[] [:a] [:a :left] [:a :left :c :d :e :f]
             [:a :b :c :d]]]
    (is (= ::fail (try (i/join {} i) (catch Exception e ::fail)))))
  (is (= [{} " \"t1\" left join \"t2\" "]
         (i/join {} [:join :left :t1 :t2])))
  (is (= [{} " \"t1\" right join \"t2\" "]
         (i/join {} [:join :right :t1 :t2])))
  (is (= [{} " \"t1\" inner join \"t2\" "]
         (i/join {} [:join :inner :t1 :t2])))
  (is (= [{} " \"t1\" full join \"t2\" "]
         (i/join {} [:join :full :t1 :t2])))
  (is (= [{:t1 :table1 :t2 :table2} "  \"table1\" as \"t1\"  left join  \"table2\" as \"t2\"  "]
         (i/join {} [:join :left {:t1 :table1} {:t2 :table2}]))))

(deftest from-map
  (is (= ::fail
         (try (i/from-map {} {}) (catch Exception e ::fail))))
  (is (= [(sorted-map :a :b :c :d) " \"b\" as \"a\" , \"d\" as \"c\" "]
         (i/from-map (sorted-map) {:a :b :c :d}))))

;; :sample
(deftest from-vec
  (doseq [i [[:only] [:only :a :b] [:only 123]
             [:join] [:join :a :b] [:join :a :b :c :d :e]]]
    (is (= ::fail
           (try (i/from-vec {} i) (catch Exception e ::fail)))))
  (is (= [{} " only \"a\" "]
         (i/from-vec {} [:only :a])))
  (is (= [{:t1 :table1 :t2 :table2} "  \"table1\" as \"t1\"  left join  \"table2\" as \"t2\"  "]
         (i/join {} [:join :left {:t1 :table1} {:t2 :table2}]))))

(deftest from-item
  (is (= [{} (u/kw->ident :t1)] (i/from-item {} :t1) ))
  (is (= [(sorted-map :a :b :c :d) " \"b\" as \"a\" , \"d\" as \"c\" "]
         (i/from-item (sorted-map) {:a :b :c :d})))
  (is (= [{:t1 :table1 :t2 :table2} "  \"table1\" as \"t1\"  left join  \"table2\" as \"t2\"  "]
         (i/from-item {} [:join :left {:t1 :table1} {:t2 :table2}])))
  (is (= [{} "123"]
         (i/from-item {} 123)))
  (is (= [{} "current_timestamp"]
         (i/from-item {} (i/->Literal "current_timestamp"))))
  (is (= [{} "\"now\"(  )"]
         (i/from-item {} :now<>)))
  (is (= [{:ts ::i/none} " \"now\"(  ) as \"ts\" "]
         (i/from-item {} {:ts :now<>}))))

(deftest cast
  (doseq [i [[] [:a] [:a :b :c]]]
    (is (= ::fail (try (i/cast {} i) (catch Exception e ::fail)))))
  (is (= "( \"foo\" :: \"bar\" )" (i/cast {} [:foo :bar]))))
(deftest and*
  (is (= "true" (i/and* {} [])))
  (is (= "( \"foo\" )" (i/and* {} [:foo])))
  (is (= "( \"foo\" and \"bar\" )" (i/and* {} [:foo :bar])))
  (is (= "( \"foo\" and \"bar\" and \"baz\" )" (i/and* {} [:foo :bar :baz]))))
(deftest or*
  (is (= "true" (i/or* {} [])))
  (is (= "( \"foo\" )" (i/or* {} [:foo])))
  (is (= "( \"foo\" or \"bar\" )" (i/or* {} [:foo :bar])))
  (is (= "( \"foo\" or \"bar\" or \"baz\" )" (i/or* {} [:foo :bar :baz]))))
(deftest null?
  (doseq [i [[] [:a :b]]]
    (is (= ::fail (try (i/null? {} i) (catch Exception e ::fail)))))
  (is (= "( \"foo\" is null )" (i/null? {} [:foo]))))
(deftest not-null?
  (doseq [i [[] [:a :b]]]
    (is (= ::fail (try (i/not-null? {} i) (catch Exception e ::fail)))))
  (is (= "( \"foo\" is not null )" (i/not-null? {} [:foo]))))
(deftest funcall
   (is (= "\"now\"(  )" (i/funcall {} :now<> []))))
   (is (= "\"now\"( 123 )" (i/funcall {} :now<> [123])))
(deftest op
  (is (= "not 123" (i/op {} :not [123])))
  (is (= "foo 123" (i/op {} :foo [123])))
  (is (= "123 like 456" (i/op {} :like [123 456])))
  (is (= "123 -> 456 -> 789" (i/op {} :-> [123 456 789]))))

(deftest expr
  (is (= "foo"   (i/expr {} (i/->Literal "foo"))))
  (is (= "123"   (i/expr {} 123)))
  (is (= "true"  (i/expr {} true)))
  (is (= "false" (i/expr {} false)))
  (is (= (u/kw->ident :foo/bar) (i/expr {} :foo/bar)))
  (is (= "123 -> 456 -> 789" (i/expr {} [:-> 123 456 789]))))
