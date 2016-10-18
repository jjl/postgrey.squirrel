(ns postgrey.sql-test
  (:require [postgrey.sql :as sql]
            [clojure.spec :as s :include-macros true]
   #?(:clj  [clojure.test :refer [deftest testing is]]
      :cljs [cljs.test :refer-macros [deftest testing is]])))

(def emp (sql/new-state))

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
  (is (= [123 2] (sql/build-some [::mock 123] 1)))
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

;; (t/deftest records
;;   (let [l  (s/lit "foo")
;;         p1 (s/hold "place")
;;         p2 (s/? "place")]
;;     (t/is (every? true? [(s/literal? l) (s/placeholder? p1) (s/placeholder? p2)]))
;;     (doseq [f [s/literal? s/placeholder?]]
;;       (t/testing f
;;         (t/is (every? false? (map f ["foo" 123 1.23 [] {}])))))
;;     (t/is (= "foo"   (:val l)))
;;     (t/is (= "place" (:name p1) (:name p2)))))

;; (t/deftest a-simple
;;   (let [e-keys #{:error :item :source}
;;         f1 (s/a-simple :a emp)
;;         f2 (s/a-simple ::foo :a emp)]
;;     (t-error f1 "Invalid simple item" ::s/a-simple e-keys)
;;     (t-error f2 "Invalid simple item" ::foo e-keys)
;;     (doseq [t [123 1.23 true false]]
;;       (t/testing t
;;         (let [r1 (s/a-simple t emp)
;;               r2 (s/a-simple ::foo t emp)]
;;           (doseq [r [r1 r2]]
;;             (t-appends r [(str t)])
;;             (t-good r)))))))

;; (t/deftest a-literal
;;   (let [e-keys #{:error :literal :source}
;;         f1 (s/a-literal :a emp)
;;         f2 (s/a-literal ::foo :a emp)
;;         f3 (s/a-literal (s/lit "") emp)
;;         f4 (s/a-literal ::foo (s/lit "") emp)
;;         r1 (s/a-literal (s/lit "foo") emp)
;;         r2 (s/a-literal ::foo (s/lit "foo") emp)]
;;     (t-error f1 "Expected literal" ::s/a-literal e-keys)
;;     (t-error f2 "Expected literal" ::foo e-keys)
;;     (t-error f3 "Empty literal" ::s/a-literal e-keys)
;;     (t-error f4 "Empty literal" ::foo e-keys)
;;     (doseq [r [r1 r2]]
;;       (t-appends r ["foo"])
;;       (t-good r))))

;; (t/deftest a-keyword-qualified
;;   (let [e-msg "Invalid qualified keyword"
;;         e-keys #{:error :keyword :source}]
;;     (doseq [t [123 1.23 "a" [] {} () nil]]
;;       (let [f1 (s/a-keyword-qualified t emp)
;;             f2 (s/a-keyword-qualified ::foo t emp)]
;;         (t-error f1 e-msg ::s/a-keyword-qualified e-keys)
;;         (t-error f2 e-msg ::foo e-keys)))
;;     (doseq [[t e] [[:a "\"a\""] [:a/b "\"a\".\"b\""] [:a.b/c "\"a\".\"b\".\"c\""] [:a.b/* "\"a\".\"b\".\"*\""]]]
;;       (t/testing t
;;         (let [r1 (s/a-keyword-qualified t emp)
;;               r2 (s/a-keyword-qualified ::foo t emp)]
;;           (doseq [r [r1 r2]]
;;             (t-appends r [e])
;;             (t-good r)))))))

;; (t/deftest a-keyword-wildcard
;;   (let [e-msg "Invalid qualified keyword"
;;         e-keys #{:error :keyword :source}]
;;     (doseq [t [123 1.23 "a" [] {} () nil]]
;;       (t/testing t
;;         (let [f1 (s/a-keyword-wildcard t emp)
;;               f2 (s/a-keyword-wildcard ::foo t emp)]
;;           (t-error f1 e-msg ::s/a-keyword-wildcard e-keys)
;;           (t-error f2 e-msg ::foo e-keys))))
;;     (doseq [[t e] [[:a "\"a\""] [:a/b "\"a\".\"b\""] [:a.b/c "\"a\".\"b\".\"c\""] [:a.b/* "\"a\".\"b\".*"]]]
;;       (t/testing t
;;         (let [r1 (s/a-keyword-wildcard t emp)
;;               r2 (s/a-keyword-wildcard ::foo t emp)]
;;           (doseq [r [r1 r2]]
;;             (t-appends r [e])
;;             (t-good r)))))))

;; (t/deftest a-ident-string
;;   (let [e-msg "Invalid identifier string"
;;         e-keys #{:error :ident :source}
;;         r1 (s/a-ident-string "a.b\"/c" emp)
;;         r2 (s/a-ident-string ::foo "a.b\"/c" emp)]
;;     (doseq [t [123 1.23 :a/b [] {} () nil]]
;;       (t/testing t
;;         (let [f1 (s/a-ident-string t emp)
;;               f2 (s/a-ident-string ::foo t emp)]
;;           (t-error f1 e-msg ::s/a-ident-string e-keys)
;;           (t-error f2 e-msg ::foo e-keys))))
;;     (doseq [r [r1 r2]]
;;       (t-appends r ["\"a.b\"\"/c\""])
;;       (t-good r))))

;; (t/deftest a-symbol
;;   (let [e-msg "Invalid symbol"
;;         e-keys #{:error :symbol :source :valid}]
;;     (doseq [t [123 1.23 :a/b [] {} () nil]]
;;       (t/testing t
;;         (let [f1 (s/a-symbol t emp)
;;               f2 (s/a-symbol ::foo t emp)]
;;           (t-error f1 e-msg ::s/a-symbol e-keys)
;;           (t-error f2 e-msg ::foo e-keys))))
;;     (doseq [t ['ba.r-baz 'foo/ba.r-baz]]
;;       (let [r1 (s/a-symbol t emp)
;;             r2 (s/a-symbol ::foo t emp)]
;;         (doseq [r [r1 r2]]
;;           (t-appends r ["bar_baz"])
;;           (t-good r))))))

;; (t/deftest a-keyword-unqualified
;;   (let [e-keys #{:error :source :keyword}
;;         e-msg "Expected keyword without namespace"
;;         r1 (s/a-keyword-unqualified :foo emp)]
;;     (doseq [t [:foo/bar "a" 'a 123 1.23 {} [] ()]]
;;       (t/testing t
;;         (t-error (s/a-keyword-unqualified t emp) e-msg ::s/a-keyword-unqualified e-keys)
;;         (t-error (s/a-keyword-unqualified ::foo t emp) e-msg ::foo e-keys)))
;; <    (t-appends r1 ["\"foo\""])))

;; (t/deftest a-ident-atomic
;;   (let [e-keys #{:error :identifier :source :valid}
;;         e-msg "Invalid identifier atom"
;;         r (reduce #(s/a-ident-atomic %2 %1) emp [(s/lit "foo") "bar" :baz 'quux])]
;;     (with-redefs [s/a-literal (constantly ::foo)
;;                   s/a-ident-string (constantly ::foo)
;;                   s/a-keyword-unqualified (constantly ::foo)]
;;       (doseq [i [(s/lit "") "" :foo]]
;;         (t/testing i
;;           (t/is (= ::foo (s/a-ident-atomic i emp))))))
;;     (t/is (t-good r))
;;     (t-appends r ["foo" "\"bar\"" "\"baz\"" "quux"])
;;     (doseq [t [123 1.23 [] {} () nil :foo/bar 'a/b]]
;;       (t/testing t
;;         (let [f1 (s/a-ident-atomic t emp)
;;               f2 (s/a-ident-atomic ::foo t emp)]
;;           (t-error f1 e-msg ::s/a-ident-atomic e-keys)
;;           (t-error f2 e-msg ::foo e-keys))))))

;; (t/deftest a-ident-qualified ;;don't forgot to add back into a-ident tests
;;   )

;; (t/deftest a-ident
;;   (let [e-keys #{:error :identifier :source :valid}
;;         e-msg "Invalid identifier"
;;         r (reduce #(s/a-ident %2 %1) emp [(s/lit "foo.bar") "bar/baz" :baz/quux 'quux])]
;;     (with-redefs [s/a-literal (constantly ::foo)
;;                   s/a-ident-string (constantly ::foo)
;;                   s/a-keyword-qualified (constantly ::foo)]
;;       (doseq [i [(s/lit "") "" :foo]]
;;         (t/testing i
;;           (t/is (= ::foo (s/a-ident i emp))))))
;;     (t/is (t-good r))
;;     (t-appends r ["foo.bar" "\"bar/baz\"" "\"baz\".\"quux\"" "quux"])
;;     (doseq [t [123 1.23 [] {} () nil 'baz/quux]]
;;       (t/testing t
;;         (let [f1 (s/a-ident t emp)
;;               f2 (s/a-ident ::foo t emp)]
;;           (t-error f1 e-msg ::s/a-ident e-keys)
;;           (t-error f2 e-msg ::foo e-keys))))))


;; ;; (t/deftest a-as
;; ;;   (let [e-keys #{:error :valid :source :alias}
;; ;;         e-msg "Invalid alias"

;; ;; a-col-def
