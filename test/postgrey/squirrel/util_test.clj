(ns postgrey.squirrel.util-test
  (:use [clojure.test])
  (:require [postgrey.squirrel.util :as u])
  (:import [java.lang Exception]))

(deftest kw->ident
  (is (= "\"foo\"" (u/kw->ident :foo)))
  (is (= "\"foo\".\"bar\"" (u/kw->ident :foo/bar)))
  (is (= "\"bar \"\"foo\"\" \".\"\"\"baz\""
         (u/kw->ident (keyword "bar \"foo\" /\"baz")))))

(deftest boolean?
  (is (true? (u/boolean? true)))
  (is (true? (u/boolean? false)))
  (doseq [i [1 3.0 "" :a \a () [] #{} {}]]
    (is (false? (u/boolean? i)))))

(deftest kw-chop
  (is (= :foo (u/kw-chop :foo<>)))
  (is (= :foo/bar (u/kw-chop :foo/bar<>)))
  (is (= (keyword "foo \"bar\"///baz")
         (u/kw-chop (keyword "foo \"bar\"///baz<>")))))

(deftest kw->ident)


