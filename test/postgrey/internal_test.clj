(ns postgrey.internal-test)
;;   (:use [clojure.test])
;;   (:require [postgrey.squirrel.internal :as i]
;;             [postgrey.squirrel.state :as s]
;;             [postgrey.squirrel.util :as u])
;;   (:import [java.lang Exception])
;;   (:refer-clojure :exclude [cast]))

;; (def es s/empty-state)

;; (deftest literal?
;;   (is (= true  (i/literal? (i/->Literal "foo"))))
;;   (is (= false (i/literal? "foo")))
;;   (is (= false (i/literal? 123)))
;;   (is (= false (i/literal? [])))
;;   (is (= false (i/literal? {}))))

;; (deftest from-on
;;   (is (= [es ""]           (i/from-on es nil)))
;;   (is (= [es " on true"]  (i/from-on es true))))
;;   ;; (is (= [es " on \"a\" = \"b\" "] (i/from-on es [:= :a :b]))))

;; (deftest join-cond
;;   (is (= [es nil]
;;          (i/join-cond es nil)))
;;   (is (= [es " natural"]
;;          (i/join-cond es :natural)))
;;   (is (= [es " on true"]
;;          (i/join-cond es [:on true])))
;;   (is (= [es " using (\"a\",\"b\")"]
;;          (i/join-cond es [:using :a :b]))))

;; ;; ;; todo: lateral
;; ;; ;; todo: subquery aliases
;; (deftest join
;;   (doseq [i [[] [:a] [:a :left] [:a :left :c :d :e :f]
;;              [:a :b :c :d]]]
;;     (is (= ::fail (try (i/join es i) (catch Exception e ::fail)))))
;;   (is (= [es " \"t1\" left join \"t2\" "]
;;          (i/join es [:join :left :t1 :t2])))
;;   (is (= [es " \"t1\" right join \"t2\" "]
;;          (i/join es [:join :right :t1 :t2])))
;;   (is (= [es " \"t1\" inner join \"t2\" "]
;;          (i/join es [:join :inner :t1 :t2])))
;;   (is (= [es " \"t1\" full join \"t2\" "]
;;          (i/join es [:join :full :t1 :t2])))
;;   (is (= [es "  \"table1\" as \"t1\"  left join  \"table2\" as \"t2\"  "]
;;          (i/join es [:join :left {:t1 :table1} {:t2 :table2}]))))

;; (deftest from-map
;;   (is (= ::fail
;;          (try (i/from-map es {}) (catch Exception e ::fail))))
;;   (is (= [es " \"b\" as \"a\" , \"d\" as \"c\" "]
;;          (i/from-map es (sorted-map :a :b :c :d)))))

;; (deftest funcall
;;   (is (= [es "\"now\"()"]    (i/funcall es :now<> [])))
;;   (is (= [es "\"now\"(123)"] (i/funcall es :now<> [123]))))

;; (deftest from-item
;;   (is (= [es (u/kw->ident :t1)] (i/from-item es :t1) ))
;;   (is (= [es " \"b\" as \"a\" , \"d\" as \"c\" "]
;;          (i/from-item es (sorted-map :a :b :c :d))))
;;   (is (= [es "  \"table1\" as \"t1\"  left join  \"table2\" as \"t2\"  "]
;;          (i/from-item es [:join :left {:t1 :table1} {:t2 :table2}])))
;;   (is (= [es "123"]
;;          (i/from-item es 123)))
;;   (is (= [es "current_timestamp"]
;;          (i/from-item es (i/->Literal "current_timestamp"))))
;;   (is (= [es "\"now\"()"]
;;          (i/from-item es :now<>)))
;;   (is (= [es " \"now\"() as \"ts\" "]
;;          (i/from-item es {:ts :now<>})))
;;   (is (= [es " only \"a\" "]
;;          (i/from-item es [:only :a]))))
;; ;; :sample

;; (deftest logop
;;   (is (= [es "true"]                              (i/logop es "and" [])))
;;   (is (= [es "(\"foo\")"]                         (i/logop es "and" [:foo])))
;;   (is (= [es "(\"foo\" and \"bar\")"]             (i/logop es "and" [:foo :bar])))
;;   (is (= [es "(\"foo\" and \"bar\" and \"baz\")"] (i/logop es "and" [:foo :bar :baz]))))

;; (deftest postop
;;   (is (= [es "\"a\" is null"]
;;          (i/postop es "is null" :a)))
;;   (is (= [es "\"a\" is not null"]
;;          (i/postop es "is not null" :a)))
;;   (is (= [es "123 !"]
;;          (i/postop es "!" 123))))

;; (deftest op
;;   (is (= [es "not 123"] (i/op es :not [123])))
;;   (is (= [es "foo 123"] (i/op es :foo [123])))
;;   (is (= [es "123 like 456"] (i/op es :like [123 456])))
;;   (is (= [es "123 -> 456 -> 789"] (i/op es :-> [123 456 789]))))

;; (deftest expr-kw-vec
;;   (is (= [es "foo 123"] (i/expr-kw-vec es :foo [123])))
;;   ;; TODO: figure out namespaced operators
;;   ;; (is (= [es "123 \"foo\".\"bar\" 456"] (i/expr-kw-vec es :foo/bar [123 456])))
;;   (is (= [es "\"foo\"()"] (i/expr-kw-vec es :foo<> [])))  (is (= [es "\"foo\".\"bar\"()"] (i/expr-kw-vec es :foo/bar<> [])))
;;   (is (= [es "\"foo\"(123,456)"] (i/expr-kw-vec es :foo<> [123 456])))
;;   (is (= [es "\"foo\".\"bar\"(123,456)"] (i/expr-kw-vec es :foo/bar<> [123 456]))))

;; (deftest expr-kw
;;   (is (= [es "\"foo\"()"]         (i/expr-kw es :foo<> [])))
;;   (is (= [es "\"foo\".\"bar\"()"] (i/expr-kw-vec es :foo/bar<> [])))
;;   (is (= [es "\"foo\"(123,456)"]  (i/expr-kw es :foo<> [123 456])))
;;   (is (= [es "\"foo\""]           (i/expr-kw es :foo [])))
;;   (is (= [es "\"foo\""]           (i/expr-kw es :foo [123]))))

;; (deftest expr
;;   (is (= [es "\"bar \"\"foo\"\" \".\"\"\"baz\""]
;;          (i/expr es (keyword "bar \"foo\" /\"baz"))))
;;   (is (= [es "(\"foo\" :: bar)"]
;;          (i/expr es [:cast :foo "bar"])))
;;   (is (= [es "(\"foo\" or \"bar\")"]
;;          (i/expr es [:or  :foo :bar])))
;;   (is (= [es "(\"foo\" and \"bar\" and \"baz\")"]
;;          (i/expr es [:and :foo :bar :baz])))
;;   (is (= [es "123"]                 (i/expr es 123)))
;;   (is (= [es "true"]                (i/expr es true)))
;;   (is (= [es "false"]               (i/expr es false)))
;;   (is (= [es "foo"]                 (i/expr es (i/->Literal "foo"))))
;;   (is (= [es "\"foo\".\"bar\""]     (i/expr es :foo/bar)))
;;   (is (= [es "\"a\" is null"]     (i/expr es [:null? :a])))
;;   (is (= [es "\"a\" is not null"] (i/expr es [:not-null? :a])))
;;   (is (= [es "\"now\"()"]           (i/expr es :now<>)))
;;   (is (= [es "\"now\"()"]           (i/expr es [:now<>])))
;;   (is (= [es "\"now\"(123,456)"]           (i/expr es [:now<> 123 456])))
;;   (is (= [es "123 -> 456 -> 789"] (i/expr es [:-> 123 456 789]))))

;; (deftest exprs)
;; (deftest group-by-vec)
;; (deftest having-cause)
;; (deftest limit)
;; (deftest offset)
;; (deftest select-expr)
;; (deftest select-clause)
