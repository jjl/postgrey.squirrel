(ns postgrey.squirrel
  (:require [clojure.core.match :refer [match]]
            [clojure.spec :as s]
            [flatland.ordered.set :as os]
            [postgrey.util :as u]))

;; (s/def ::field (s/and keyword? namespace))
;; (s/def ::bind (s/and symbol? u/binding-symbol?))
;; (s/def ::arg     (s/and symbol? u/no-ns? #(not (u/binding-symbol? %))))
;; (s/def ::value   (s/or :number number? :string string? :keyword keyword? :bind ::bind :arg ::arg))
;; (s/def ::args  (s/coll-of symbol? :kind vector?))
;; (s/def ::find  (s/coll-of symbol? :kind vector?))
;; (s/def ::find-clause (s/cat :op #{:find} :names ::find))
;; (s/def ::binding (s/cat :field keyword? :val ::value))
;; (s/def ::cond (s/cat :op symbol? :args (s/+ ::value)))
;; (s/def ::bindings (s/coll-of ::binding :kind vector?))
;; (s/def ::where (s/coll-of ::cond :kind vector?))
;; (s/def ::query (s/cat :args ::args :find ::find-clause :where (s/? ::where) :order (s/? ::order)))

;; (defn build-plan* [& args]
;;   (match (s/conform ::query args)
;;     ... ...))

;; (defrecord State [args find binds where order-by])

;; (defmacro build-plan [& args]
;;   `(build-plan* ~@(map #(list 'quote %) args)))

;; (build-plan [city-id min-size avail budget]
;;  :find [?*listing]
;;  :bind [[:listing/kind :monthly]
;;         [:listing/city city]
;;         [:listing/* ?*listing]
;;         [:listing/size ?size]
;;         [:listing/available-from ?avail]
;;         [:listing/cost ?cost]
;;         [:city/id city-id]
;;         [:city/*  ?*city]]
;;  :where [[>= ?size min-size]
;;          [<= ?avail avail]
;;          [<= ?cost budget]]
;;  :order-by [[?cost :asc]])

;; (query [em pass]
;;  :find  [?*user]
;;  :where [[:user/email ?em]
;;          [:user/password ?pass]
;;          [:user/* ?*user]]
;; (select :args [em pass]
;;         :fields [?*user]
;;         :select []
;;         :order 
;;         :limit 1
;;         :offset 1
        
