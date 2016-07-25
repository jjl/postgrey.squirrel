(ns postgrey.util-test
  (:require [postgrey.util :as u]
            [clojure.test :as t])
  (:import [java.lang Exception]))

(t/deftest error-throwing
  (t/testing :fatal
  (let [e (try (u/fatal "foo" {:foo :bar}) (catch clojure.lang.ExceptionInfo e e))]
    (t/is (= "foo {:foo :bar}"(.getMessage e)))
    (t/is (= {:foo :bar} (ex-data e)))))
  (t/testing :badarg
    (let [e (try (u/badarg "foo bar") (catch java.lang.IllegalArgumentException e e))]
      (t/is (= "foo bar" (.getMessage e))))))

(t/deftest predicates
  (t/testing :named?
    (t/is (= true (u/named? :foo) (u/named? 'foo)))
    (t/is (every? false? (map u/named? [true 1 1.2 (/ 1 2) nil]))))
  (t/testing :simple?
    (t/is (every? true?  (map u/simple? [true 123 1.23])))
    (t/is (every? false? (map u/simple? [[] {} "" :a nil]))))
  (t/testing :fny?
    (t/is (every? true?  (map u/fny? [:a (constantly true) {} [] 'a])))
    (t/is (every? false? (map u/fny? [123 1.23 ""]))))
  (t/testing :str-has?
    (t/is (true?  (u/str-has?   "foo" "foo")))
    (t/is (false? (u/str-has?   "foo" "bar")))
    (t/is (true?  (u/str-hasnt? "foo" "bar")))
    (t/is (false? (u/str-hasnt? "foo" "foo"))))
  ;; (t/testing :null-free?
  ;;   (t/is (u/null-free? "foo bar"))
  ;;   (t/is (not (u/null-free? "foo\u0000baz")))) ; ffs
  (t/testing :valid-alias?
    (t/is (every? identity (map u/valid-alias? [:foo :foo/bar "foo/bar.baz_quux" 'foo])))
    (t/is (every? false?   (map (complement u/valid-alias?) [(keyword "foo\0 bar") "foo\0 bar" 'foo/bar]))))
  (t/testing :valid-string?
    (doseq [f [u/valid-string? u/valid-ident-string?]]
      (t/is (f "foo/bar.quux_q 123\u0000"))))
  (t/testing :undash)
  (t/testing :unalunddot))

(t/deftest reproduction
  (t/testing :repr-string
    (t/is (= "'foo''\"/bar'"    (u/repr-string "foo\0'\"/bar"))))
  (t/testing :repr-atomic
    (t/is (= "\"foo'\"\"/bar\"" (u/repr-atomic "foo'\0\"/bar"))))
  (t/testing :repr-qualified
    (t/is (= "\"a\"" (u/repr-qualified :a)))
    (t/is (= "\"a\".\"b\".\"*\"" (u/repr-qualified :a.b/*))))
  (t/testing :repr-wildcard
    (t/is (= "\"a\"" (u/repr-wildcard :a)))
    (t/is (= "\"a\".\"b\".*" (u/repr-wildcard :a.b/*))))
  (t/testing :repr-symbol
    (t/is (= "bar_baz" (u/repr-symbol 'foo/bar-baz)))))

;; do we even need these any more?
;; (t/deftest state-form
;;   (t/is (= '(cde abc 456)
;;          (u/state-form 'abc '(cde 456))))
;;   (t/is (= '(cde abc)
;;          (u/state-form 'abc '(cde))))
;;   (t/is (= '(cde abc)
;;          (u/state-form 'abc 'cde)))
;;   (t/is (= '(:cde abc)
;;          (u/state-form 'abc :cde))))
;; (t/deftest s->)
;; (t/deftest smap)
;; (t/deftest skeep)

(t/deftest match->)
(t/deftest match->>)
