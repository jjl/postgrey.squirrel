(ns postgrey.util-test
  (:require [postgrey.util :as u]
            [flatland.ordered.set :as os]
            [flatland.ordered.map :as om]
            [clojure.spec :as s :include-macros true]
   #?(:clj  [clojure.test :refer [deftest testing is]]
      :cljs [cljs.test :refer-macros [deftest testing is]])))

(deftest ordered-tests
  (let [os (os/ordered-set)
        om (om/ordered-map)]
    (is (u/ordered-set? os))
    (is (u/ordered-map? om))
    (is (not (u/ordered-set? om)))
    (is (not (u/ordered-map? os)))))

(deftest binding-symbols
  (is (u/binding-symbol? '?a))
  (is (not (u/binding-symbol? 'a))))

(deftest names-and-namespaces
  (is (u/no-ns? :a))
  (is (not (u/no-ns? :a/b)))
  (is (u/has-name? :a))
  (is (not (u/has-name? (keyword "a" "")))))

(deftest boilerplate
  (is (= ["a b" 1]    (u/prefix "a" ["b" 1])))
  (is (= ["(a)" 1]    (u/bracket ["a" 1])))
  (is (= ["(a)" 1]    (u/maybe-bracket ["a" 1] true)))
  (is (= ["a"   1]    (u/maybe-bracket ["a" 1] false)))
  (is (= "\"\""       (u/render-string "" \")))
  (is (= "''"         (u/render-string "" \')))
  (is (= "\"a\"\"b\"" (u/render-string "a\"b" \")))
  (is (= "'a''b'"     (u/render-string "a'b" \')))
  (is (= "\"a\"" (u/render-ident :a)))
  (is (= "\"a\".\"b\"" (u/render-ident :a/b)))
  (is (= "\"a\".\"b\".\"c\"" (u/render-ident :a.b/c)))
  (is (= "*" (u/render-wildcard :*)))
  (is (= "\"a\".*" (u/render-wildcard :a/*)))
  (is (= "\"a\".\"b\".*" (u/render-wildcard :a.b/*)))
  (is (= "a b c" (u/undashed-name "a-b c")))
  )

(deftest fatal
  (let [e (try (u/fatal "foo" {:foo :bar})
               (catch #?(:clj Exception :cljs js/Object) e e))]
    (is (= "foo\n {:foo :bar}"(.getMessage e)))
    (is (= {:foo :bar} (ex-data e)))))

(deftest names
  (testing :named
    (is (u/named? 'a))
    (is (u/named? :a))
    (is (every? not (map u/named? [true 1 1.2 (/ 1 2) nil]))))
  (testing :name=/name-not=
    (let [f1 (u/name= "foo")
          f2 (u/name-not= "foo")]
      (is (fn? f1))
      (is (fn? f2))
      (is (f1 :a/foo))
      (is (f1 :foo))
      (is (not (f1 :bar)))
      (is (f2 :a/bar))
      (is (f2 :bar))
      (is (not (f2 :foo))))))

(deftest fny?
  (is (every? true?  (map u/fny? [:a (constantly true) {} [] 'a])))
  (is (every? false? (map u/fny? [123 1.23 ""]))))

