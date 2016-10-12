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
       (not (namespace s))
       (.startsWith ^String (name s) "?")))

(defn make-binding-symbol [s]
  (symbol (str "?" (name s))))

(defn no-ns? [v]
  (not (namespace v)))

(defn has-name? [v]
  (not= "" (name v)))

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
  (update v 0 #(str "(" % ")")))

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
  "An ident is a keyword or a symbol. Renders it using the namespace
   if any as part of the output
   args: v"
  [v]
  (let [nss (str/split (or (namespace v) "") #"\.")]
    (->> (conj nss (name v))
         (into [] (comp (filter #(not= "" %)) (map #(render-string % \"))))
         (str/join "."))))

(defn undashed-name [val]
  (str/replace (name val) \- \space))

;; hierarchy manip

(defn leaf-descendants
  "Returns only the leaf descendants of tag from the global hierarchy"
  [tag]
  (->> tag descendants
       (into #{} (filter #(not (seq (descendants %)))))))

;; throwing errors

(defn fatal
  "Throws an exception with message and data. Appends printed data to message
   args: [msg data]
   returns: nothing, throws!"
  [msg data]
  (throw (ex-info (str msg \newline " " (pr-str data)) data)))

(defn badarg [msg]
  #?(:clj (throw (IllegalArgumentException. msg))
     :cljs (throw (ex-info (str "Illegal argument: " msg) {}))))

;; predicates

(defn one-or-all [c]
  (if (next c)
    (first c)
    c))

(defn named?
  "true if the given argument is clojure.lang.Named
   args: [subj]
   returns: bool"
  [subj]
  (instance? clojure.lang.Named subj))

(def simple?
  "true if the value passed is a number or a boolean
   args: [val]
   returns: boolean"
  (some-fn boolean? number?))

(def fny? (some-fn ifn? symbol?))

(defn str-has? [n h]
  (not= -1 (.indexOf ^String n ^String h)))

(defn str-hasnt? [n h]
  (= -1 (.indexOf ^String n ^String h)))

(defn null-free? [s]
  (str-hasnt? "\u0000" s))

(defn valid-alias? [a]
  (cond (symbol?  a) (re-find #"^[a-z_]+" (str a))
        (string?  a) (null-free? a)
        (keyword? a) (null-free? (str (.-sym a)))
        :else nil))

(defn valid-string? [s]
  (and (string? s) (not= "" (str/replace s #"\x00" ""))))

(def valid-ident-string? valid-string?)

;; handy utilities

(defn undash [str]
  (str/replace str #"-" "_"))

(defn unalunddot [str]
  (str/replace str #"[^a-z.]" ""))

;; sql printers

(defn repr-string [s]
  (when (string? s)
    (let [r (-> s (str/replace #"'" "''") (str/replace #"\x00" ""))]
      (when (not= "" r)
        (str \' r  \')))))

(defn repr-atomic [s]
  (when (string? s)
    (let [r (-> s (str/replace #"\"" "\"\"") (str/replace #"\x00" ""))]
      (when (not= "" r)
        (str \" r \")))))

(defn repr-qualified [k]
  (when (keyword? k)
    (let [ns (filter #(not= "" %)
                     (-> k namespace (or "") undash unalunddot (str/split #"\.")))
          r (->> k name repr-atomic
                 (conj (into [] (map repr-atomic ns)))
                 (str/join "."))]
    (when (not= "" r))
      r)))

(defn repr-wildcard [k]
  (when (keyword? k)
    (let [ns (filter #(not= "" %) (-> k namespace (or "") undash unalunddot (str/split #"\.")))
          n  (name k)
          r  (->> (conj (into [] (map repr-atomic ns))
                        (if (= "*" n)
                          "*"
                          (repr-atomic n)))
                  (str/join "."))]
      (when (not= "" r)
        r))))

(defn repr-symbol [s]
  (when (symbol? s)
    (let [r (-> s name (str/replace #"-" "_") (str/replace #"[^a-z_]" ""))]
      (when (not= "" r)
        r))))

(defn state-form
  ""
  [sym f]
  (cond (list? f) `(~(first f) ~sym ~@(rest f))
        (fny? f)  (list f sym)
        :else (throw (ex-info (str "Don't know what to do with " f) {:got f}))))

(defmacro s->
  "[macro] Threads state through functions
   args: [init & clauses]
     init is the initial state value
     clauses look like -> clauses, either functions or lists.
     like ->, we insert the state into the second position
   returns: [final-state returns]
     returns is a vector of the return values in order of threading"
  [init & clauses]
  (if (seq clauses)
    (let [sym `state#
          ns (repeatedly (count clauses) #(gensym "r"))
          binds (map (fn [r] [r sym]) ns)
          forms (map #(state-form sym %) clauses)
          lets (interleave binds forms)]
      `(let [~sym ~init ~@lets]
         [[~@ns] ~sym]))
    init))

;; (defn smap
;;   ""
;;   [f state coll]
;;   (loop [st state
;;          rs (transient [])
;;          cs coll]
;;     (if (seq cs)
;;       (let [[c & cs_] cs
;;             [st2 r] (f st c)]
;;         (recur st2 (conj! rs r) cs_))
;;       [st (persistent! rs)])))

;; (defn skeep
;;   ""
;;   [f state coll]
;;   (into [] (keep identity (smap f state coll))))

;; match macros

(defmacro match-> [expr & clauses]
  (let [name `match->#]
    `(as-> ~expr ~name
       ~@(-> #(match %
                ([a] :seq)   (badarg "match-> takes an even number of clauses")
                ([a b] :seq) (if (= '_ a)
                               (match b
                                 ([a & rest] :seq) `(~a ~name ~@rest)
                                 :else (list b name))
                               `(match ~name ~a
                                 ~(match b
                                    ([a & rest] :seq) `(~a ~name ~@rest)
                                    :else (list b name))
                                 :else ~name)))
             (map (partition-all 2 clauses))))))

(defmacro match->> [expr & clauses]
  (let [name `gen#]
    `(as-> ~expr ~name
       ~@(-> #(match %
                ([a] :seq)   (badarg "match-> takes an even number of clauses")
                ([a b] :seq) (if (= '_ a)
                               (match b
                                 ([& rest] :seq) `(~@rest ~name)
                                 :else (list b name))
                               `(match ~name ~a
                                  ~(match b
                                     ([& rest] :seq) `(~@rest ~name)
                                     :else (list b name))
                                  :else ~name)))
             (map (partition-all 2 clauses))))))

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
