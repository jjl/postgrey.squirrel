(ns postgrey.util
  (:require [clojure.string :as str]
            [flatland.ordered.set :as os]
            [flatland.ordered.map :as om]
            [clojure.core.match :refer [match]])
  (:import [clojure.lang Keyword]))

;; ordered

(defn ordered-set?
  "true if given an ordered set
   args: [s]
   returns: bool"
  [s]
  (instance? flatland.ordered.set.OrderedSet s))

(defn ordered-map?
  "true if given an ordered map
   args: [m]
   returns: bool"
  [m]
  (instance? flatland.ordered.map.OrderedMap m))

;; binding symbols

(defn binding-symbol? [s]
  (and (symbol? s)
       (not= '? s)
       (.startsWith ^String (name s) "?")))

(defn make-binding-symbol [s]
  (symbol nil (str "?" (name s))))

;; namespaces

(defn no-ns? [v]
  (empty? (namespace v)))

(defn has-name? [v]
  (seq (name v)))

(defn prefix [pre val]
  (update val 0 #(str pre " " %)))

(def reserved-keywords
  (into [(symbol "false") (symbol "true")]
    '[all analyse analyze and any array as asc asymmetric authorization binary
      both case cast check collate collation concurrently constraint create cross
      current_catalog current_date current_role current_schema current_timestamp
      current_user default deferrable desc distinct do else end except fetch for
      foreign from full grant group having ilike in initially inner intersect
      into is isnull join lateral leading left like limit localtime localtimestamp
      natural not notnull null offset on only or order outer overlaps placing primary
      references returning right select session_user similar some symmetric table
      tablesample then to trailing union unique user using variadic verbose when
      where window with]))

(defn bracket [v]
  (update v 0 #(if % (str "(" % ")") "")))

(defn maybe-bracket
  "if bracket?, wrap val in parens
   args: [val bracket?]
     val: string
     bracket?: bool
   return: string"
  [val bracket?]
  (if bracket?
    (bracket val)
    val))

(defn render-string
  "Renders a string with the given delimiter, using doubling for escape
   args: [str delim]
     str: the string to render
     delim: the string delimiter, should be a character or length-1 string
   returns: string"
  [string delim]
  (str delim (.replaceAll ^String string (str delim) (str delim delim)) delim))

(defn render-ident
  "Renders an identifier keyword using the namespace
   if any as part of the output (split on '.')
   args: v
   returns: string"
  [v]
  (match [(namespace v) v]
    [nil _] (render-string (name v) \")
    [ns  s] (->> (conj (str/split ns #"\.") (name v))
                 (into [] (comp (filter #(not= "" %)) (map #(render-string % \"))))
                 (str/join "."))))

(defn wildcard? [k]
  (and (keyword? k)
       (= "*" (name k))
       (not (re-find #"^(?:\.+|.*\.\..*)$" (or (namespace k) "")))))

(defn render-wildcard
  "Renders a wildcard keyword using the namespace
   if any as part of the output (split on '.')
   args: v
   returns: string"
  [v]
  (match [(namespace v) v]
    [nil _] "*"
    [ns  s] (as-> (str/split ns #"\.") $
              (into [] (comp (filter #(not= "" %)) (map #(render-string % \"))) $)
              (str/join "." $)
              (str $ ".*"))))

(defn undashed-name [val]
  (str/replace (name val) \- \space))

;; throwing errors

(defn fatal
  "Throws an exception with message and data. Appends printed data to message
   args: [msg data]
   returns: nothing, throws!"
  [msg data]
  (throw (ex-info (str msg \newline " " (pr-str data)) data)))

;; predicates

(defn named?
  "true if the given argument is clojure.lang.Named
   args: [subj]
   returns: bool"
  [subj]
  (instance? clojure.lang.Named subj))

(defn name=
  "Returns a predicate that tests if the original value is the name
   of the new value
   args: [name-string]
   returns: fn: [ident] -> bool"
  [v]
  (fn [v2]
    (when (named? v2)
      (= v (name v2)))))

(defn name-not=
  "Inverse of name=
   args: [name-string]
   returns: fn: [ident] -> bool"
  [v]
  (complement (name= v)))

(def fny? (some-fn ifn? symbol?))

;; match macros

;; (defmacro match-> [expr & clauses]
;;   (let [name `match->#]
;;     `(as-> ~expr ~name
;;        ~@(-> #(match %
;;                 ([a] :seq)   (badarg "match-> takes an even number of clauses")
;;                 ([a b] :seq) (if (= '_ a)
;;                                (match b
;;                                  ([a & rest] :seq) `(~a ~name ~@rest)
;;                                  :else (list b name))
;;                                `(match ~name ~a
;;                                  ~(match b
;;                                     ([a & rest] :seq) `(~a ~name ~@rest)
;;                                     :else (list b name))
;;                                  :else ~name)))
;;              (map (partition-all 2 clauses))))))

;; (defmacro match->> [expr & clauses]
;;   (let [name `gen#]
;;     `(as-> ~expr ~name
;;        ~@(-> #(match %
;;                 ([a] :seq)   (badarg "match-> takes an even number of clauses")
;;                 ([a b] :seq) (if (= '_ a)
;;                                (match b
;;                                  ([& rest] :seq) `(~@rest ~name)
;;                                  :else (list b name))
;;                                `(match ~name ~a
;;                                   ~(match b
;;                                      ([& rest] :seq) `(~@rest ~name)
;;                                      :else (list b name))
;;                                   :else ~name)))
;;              (map (partition-all 2 clauses))))))

(def sorted-map? (every-pred map? sorted?))

(defn maybe-sort
  "If the given map is not sorted or ordered, sort it, else return it
   args: [map]
   returns: map"
  [m]
  (if (or (ordered-map? m)
          (sorted-map? m))
    m
    (into (sorted-map) m)))
