(ns postgrey.state-test
  (:require [postgrey.state :as s]
            [clojure.test :as t]))

(t/deftest state-tests
  (let [emp s/empty-state
        t1 (update emp :aliases conj ::test)
        t2 (update emp :placeholders conj ::foo)
        t3 (update t2  :aliases conj ::test)
        t4 (update emp :acc conj ::foo ::bar)
        t5 (-> emp (update :errors conj ::foo) (update :acc conj "<ERROR #0>"))
        t6 (-> t5  (update :errors conj ::bar) (update :acc conj "<ERROR #1>"))
        t7 (update emp :warnings conj ::foo)
        t8 (s/error t1 {:error "Duplicate name" :name ::test :source ::s/put-new :type :aliases})
        t9 (s/append t4 [::baz ::quux])]
    (t/testing :without-aliases
      (t/is (= emp (s/without-aliases t1))))
    (t/testing :replace-aliases
      (t/is (= t3 (s/replace-aliases t2 t1))))
    (t/testing :good?
      (t/is (s/good? emp))
      (t/is (not (s/good? (s/error emp {:foo :bar})))))
    (t/testing :append
      (t/is (= t4 (s/append emp [::foo ::bar]))))
    (t/testing :error
      (t/is (= t5 (s/error emp ::foo)))
      (t/is (= t6 (s/error t5 ::bar))))
    (t/testing :warning
      (t/is (= t7 (s/warning emp ::foo))))
    (t/testing :put-new
      (t/is (= t1 (s/put-new emp :aliases ::test)))
      (t/is (= t2 (s/put-new emp :placeholders ::foo)))
      (t/is (= t8 (s/put-new t1 :aliases ::test))))
    (t/testing :hold-place
      (t/is (= t2 (s/hold-place emp ::foo))))
    (t/testing :hold-alias
      (t/is (= t1 (s/hold-alias emp ::test))))
    (t/testing :prefix
      (t/is (= t4 (s/prefix ::foo #(s/append %2 %1) ::bar emp))))
    (t/testing :maybe-prefix
      (t/is (= t4 (s/maybe-prefix true ::foo #(s/append %2 %1) ::bar emp)))
      (t/is (= (s/append emp ::bar)
               (s/maybe-prefix false ::foo #(s/append %2 %1) ::bar emp))))
    (t/testing :suffix
      (t/is (= t4 (s/suffix ::bar #(s/append %2 %1) ::foo emp))))
    (t/testing :maybe-suffix
      (t/is (= t4 (s/maybe-suffix true  ::bar #(s/append %2 %1) ::foo emp)))
      (t/is (= (s/append emp ::foo) (s/maybe-suffix false ::bar #(s/append %2 %1) ::foo emp))))
    (t/testing :surround
      (t/is (= (s/append emp ::foo ::bar ::baz)
               (s/surround ::foo ::baz #(s/append %2 %1) ::bar emp))))
    (t/testing :parens
      (t/is (= (s/append emp "(" ::bar ")")
               (s/parens #(s/append %2 %1) ::bar emp))))))

