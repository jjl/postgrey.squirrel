(ns postgrey.sql-test
  (:require [postgrey.sql :as s]
            [postgrey.state :as st]
   #?(:clj  [clojure.test :as t]
      :cljs [cljs.test :as t :include-macros true])))

(defn t-appends [st vals]
  (t/is (= vals (:acc st)))
  st)
(defn t-error-msg [st msg]
  (t/is (= msg (-> st :errors peek :error)))
  st)
(defn t-error-keys [st ks]
  (t/is (= (into #{} ks)
         (into #{} (-> st :errors peek keys))))
  st)
(defn t-error-source [st src]
  (t/is (= src (-> st :errors peek :source)))
  st)
(defn t-error-free [st]
  (t/is (empty? (:errors st))))

;; (defn t-with-source [f & args]
;;   (
;;   )

(t/deftest records
  (let [l  (s/lit "foo")
        p1 (s/hold "place")
        p2 (s/? "place")]
    (t/is (every? true? [(s/literal? l) (s/placeholder? p1) (s/placeholder? p2)]))
    (doseq [f [s/literal? s/placeholder?]]
      (t/is (every? false? (map f ["foo" 123 1.23 [] {}]))))
    (t/is (= "foo"   (:val l)))
    (t/is (= "place" (:name p1) (:name p2)))))

(t/deftest a-simple
  (let [e (st/make-empty-state)]))

