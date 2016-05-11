(ns postgrey.squirrel.util-test
  (:use [clojure.test])
  (:require [postgrey.squirrel.util :as u])
  (:import [java.lang Exception]))

(deftest kw-chop
  (is (= :foo (u/kw-chop :foo)))
  (is (= :foo (u/kw-chop :foo<>)))
  (is (= :foo/bar (u/kw-chop :foo/bar)))
  (is (= :foo/bar (u/kw-chop :foo/bar<>)))
  (is (= (keyword "foo \"bar\"///baz")
         (u/kw-chop (keyword "foo \"bar\"///baz<>")))))

(deftest kw->ident
  (is (= "\"foo\"" (u/kw->ident :foo)))
  (is (= "\"foo\".\"bar\"" (u/kw->ident :foo/bar)))
  (is (= "\"bar \"\"foo\"\" \".\"\"\"baz\""
         (u/kw->ident (keyword "bar \"foo\" /\"baz")))))

(deftest funkw->ident
  (is (= "\"foo\"" (u/funkw->ident :foo)))
  (is (= "\"foo\"" (u/funkw->ident :foo<>)))
  (is (= "\"foo\".\"bar\"" (u/funkw->ident :foo/bar)))
  (is (= "\"foo\".\"bar\"" (u/funkw->ident :foo/bar<>))))

(deftest boolean?
  (is (true? (u/boolean? true)))
  (is (true? (u/boolean? false)))
  (doseq [i [1 3.0 "" :a \a () [] #{} {}]]
    (is (false? (u/boolean? i)))))

(deftest fnish-kw?
  (is (= true (u/fnish-kw? :foo<>)))
  (is (= true (u/fnish-kw? :foo/bar<>)))
  (is (= false (u/fnish-kw? :foo)))
  (is (= false (u/fnish-kw? :foo/bar))))

(deftest simple?
  (is (= true  (u/simple? true)))
  (is (= true  (u/simple? false)))
  (is (= true  (u/simple? 123)))
  (is (= false (u/simple? [])))
  (is (= false (u/simple? {})))
  (is (= false (u/simple? "")))
  (is (= false (u/simple? :a))))

(deftest fny?
  (is (u/fny? :a))
  (is (u/fny? (constantly true)))
  (is (u/fny? {}))
  (is (u/fny? []))
  (is (u/fny? 'abc))
  (is (not (u/fny? 123)))
  (is (not (u/fny? ""))))

(deftest state-form
  (is (= '(cde abc 456)
         (u/state-form 'abc '(cde 456))))
  (is (= '(cde abc)
         (u/state-form 'abc '(cde))))
  (is (= '(cde abc)
         (u/state-form 'abc 'cde)))
  (is (= '(:cde abc)
         (u/state-form 'abc :cde))))
(deftest s->)
(deftest smap)
(deftest skeep)
