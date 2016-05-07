(ns postgrey.squirrel.internal
  (:require [clojure.string :as str]
            [postgrey.squirrel.util :as u :refer [boolean?]])
  (:import [clojure.lang Keyword])
  (:refer-clojure :exclude [cast]))

(defprotocol Renderable
  (render [self] [self params]
    "Renders a query into a format suitable for passing to jdbc
     args: [self] [self params]
       if provided, params must be a map of binding name to value
     returns: [sql bindings-vec]"))

(defrecord Literal [value])
(defrecord Placeholder [name])

(defrecord Query [sql params presets]
  Renderable
  (render [q]
    (render q {}))
  (render [q ps]
    (let [ps' (merge presets ps)
          pv (mapv (fn [name]
                     (if-let [r (ps' name)]
                       r
                       (throw (ex-info (str "Missing parameter: " name) {:got ps :presets presets}))))
                   params)]
      [sql pv])))

(declare expr from-item)

(defn literal?
  "True if passed an instance of Literal
   args: [l]
   returns: bool"
  [l]
  (instance? Literal l))

(defn from-on
  "Generates the sql for an 'on' clause in a 'from'
   args: [aliases expr]
   returns: sql string"
  [aliases on]
  (when-not (nil? on)
    (str " on " (expr aliases on) " ")))

(def ^:private join-cond-err
  "a join condition must be one of the following forms:
    :natural - perform a natural join (auto-'using' all same-named columns)
    [:on cond] - conditional join
    [:using & cols] - conditional on these cols, which must exist in both tables!
    [:lateral expr]")

(defn join-cond-vec
  "Generates the sql for join conditions where the input is a vector
   args: [aliases c]
     c must take one of the following forms:
       [:on cond] - conditional join
       [:using & cols] - join both tables on these (same-named in both tables) cols with ==
       [:lateral expr] - not yet implemented
   returns: [aliases sql-string]"

  [aliases c]
  (case (first c)
    :on      (if (= (count c) 2)
               [aliases (expr aliases (nth c 1))]
               (throw (ex-info join-cond-err {:got c})))
    :using   (if (every? keyword? c)
               (str " using ( " (str/join ", " (map u/kw->ident (rest c)))" ) ")
               (throw (ex-info "using takes column arguments" {})))
    :lateral (if (= (count c) 2)
               (expr aliases (nth c 1))
               (throw (ex-info "lateral takes a single expression argument" {:got c})))
    (throw (ex-info join-cond-err {:got c}))))
                    
(defn join-cond [aliases c]
  (cond (nil? c)       [aliases ""]
        (= :natural c) [aliases " natural "]
        (vector? c) (join-cond-vec aliases c)
        :else       (throw (ex-info join-cond-err {:got c}))))

(defn join
  "Returns the sql for a join from a vector describing it
   args: [[:join dir t1 t2 & [on]]]
     dir: one of :left :right :inner :full
     t1, t2: table expression
   returns: sql string"
  [aliases v]
  (when (or (not (<= 4 (count v) 5))
            (not= :join (first v)))
    (throw (ex-info "Vectors are of the form `[:join :type :table1 :table2 & [on]]" {:got v})))
  (let [[_ d t1 t2 & [on]] v]
    (when-not (#{:left :right :inner :full} d)
      (throw (ex-info "Invalid join type, must be one of :left :right :inner :full" {:got d})))
    (let [[as2 t1_] (from-item aliases t1)
          [as3 t2_] (from-item as2 t2)]
      [(merge aliases as2 as3) (str " " t1_ " " (name d) " join " t2_ (from-on as3 on) " ")])))

(defn from-map
  "Returns the sql for an aliased from expression
   args: [aliases v]
   returns: [aliases v]"
  [aliases m]
  (when-not (seq m)
    (throw (ex-info "Empty map not permitted as from item" {:got m})))
  (letfn [(f [{:keys [aliases sql] :as acc}  k v]
            ;; suspect when we come to implement lateral this will be changed
            (when-not (keyword? k)
              (throw (ex-info "Keys in a from item map must be keywords!" {:got k})))
            (let [k2 (u/kw->ident k)
                  [as2 sql2] (from-item aliases v)
                  as3 (merge aliases as2)]
              {:aliases (if (keyword? v)
                          (->> (if (.endsWith ^String (name v) "<>")
                                 ::none
                                 v)
                          (assoc as3 k))
                          as3)
               :sql (conj! sql (str " " sql2 " as " k2 " "))}))]
    (let [init {:aliases {} :sql (transient [])}
          {:keys [aliases sql]} (reduce-kv f init m)]
      [aliases (str/join "," (persistent! sql))])))

(defn from-vec
  "Returns the sql for a from expression in the form of a vector
   args: [aliases v]
     v: vector: either [:only :table1] or [:join direction t1 t2 & [on]]"
  [aliases v]
  (when-not (seq v)
    (throw (ex-info "a from vector must be either [:only :table1] or [:join direction t1 t2 & [on]]" {:got v})))
  (let [[i & is] v]
    (case i
      :only (if (= (count v) 2)
              [aliases (str " only " (u/kw->ident (nth v 1)) " ")]
              (throw (ex-info "a from vector must be either [:only :table1] or [:join direction t1 t2 & [on]]" {:got v})))
      :join (join aliases v)
      :sample (throw (ex-info "Sorry, :sample/TABLESAMPLE is not yet implemented" {}))
      (expr aliases v))))

(defn funcall
  "Returns a sql funcall expression: `my.function(*args)`
   true when expr is not null
   args: [args]
   returns: sql string"
  [aliases fun args]
  (str (u/kw->ident (u/kw-chop fun)) "( " (str/join ", " (map (partial expr aliases) args)) " )"))

(defn from-item
  "Returns the sql for a from expression
   args: [aliases v & [query?]] type-dependent behaviour on v:
     keyword: names a table
     map: aliases a table
     vector: either [:only :table1] or [:join direction t1 t2 & [on]]
   if query? is true (default false), permits subqueries (not yet implemented)
   returns: string"
  ([aliases v]
   (from-item aliases v false))
  ([aliases v query?]
   (cond (literal? v) [aliases (:value v)]
         (keyword? v) (if (.endsWith ^String (name v) "<>")
                        [aliases (funcall aliases v [])]
                        [aliases (u/kw->ident v)])
         (map? v)     (from-map aliases v)
         (vector? v)  (if (and query? (= :q (first v)))
                        (throw (ex-info "Subqueries are not yet implemented. Sorry!" {}))
                        (from-vec aliases v))
         :else        [aliases (expr aliases v)])))

(defn cast
  "Returns a sql cast expression: `expr :: type`
   args: [[expr type]]
   returns: sql string"
  [aliases args]
  (when (not= 2 (count args))
    (throw (ex-info ":cast takes exactly two arguments: [expr type]" {:got args})))
  (str "( " (expr aliases (first args)) " :: " (expr aliases (second args)) " )"))

(defn and*
  "Returns a sql `and` expression: `expr1 and expr2 ... and exprN`
   When there are zero arguments, true, otherwise true when all args are
   args: [exprs]
   returns: sql string"
  [aliases args]
  (if (= 0 (count args))
    "true"
    (str "( " (str/join " and " (map (partial expr aliases) args)) " )")))

(defn or*
  "Returns a sql `or` expression: `expr1 or expr2 ... or exprN`
   true when any expression returns true. 0 args = true.
   args: [exprs]
   returns: sql string"
  [aliases args]
  (if (= 0 (count args))
    "true"
    (str "( " (str/join " or " (map (partial expr aliases) args)) " )")))

(defn null?
  "Returns a sql `is null` expression: `expr is null`
   true when expr is null
   args: [[expr]]
   returns: sql string"
  [aliases args]
  (when (not= 1 (count args))
    (throw (ex-info ":null? takes exactly one argument: expr" {:got args})))
  (str "( " (expr aliases (first args)) " is null )"))

(defn not-null?
  "Returns a sql `is null` expression: `expr is not null`
   true when expr is not null
   args: [[expr]]
   returns: sql string"
  [aliases args]
  (when (not= 1 (count args))
    (throw (ex-info ":null? takes exactly one argument: expr" {:got args})))
  (str "( " (expr aliases (first args)) " is not null )"))

;; [:schema/func<> :arg1 :arg2]
;; [:cast expr :type] ;; `::`
;; [:and expr ...exprN]
;; [:or expr ...exprN]
;; [:not expr]
;; [:!~]
;; [:null? expr]
;; [:not-null? expr]

(defn op
  "An operator application (or operator-like)
   If one arg, is assumed to be prefix unless it is specialcase
   If multiple args, inserted between all exprs"
  [aliases op args]
  (if (= 1 (count args))
    (str (name op) " " (expr aliases (first args)))
    (str/join (str " " (name op) " ") (map (partial expr aliases) args))))

(defn expr-vec
  ""
  [aliases [opk & args]]
  (case opk
    :cast      (cast aliases args)
    :and       (and* aliases args)
    :or        (or* aliases args)
    :null?     (null? aliases args)
    :not-null? (not-null? aliases args)
    (if (and (not= :<> opk) (.endsWith ^String (name opk) "<>"))
      (funcall aliases opk args)
      (op aliases opk args))))

(defn expr
  "Returns the sql for an expression
   args: [aliases expr]
   returns: sql string"
  [aliases v]
  (cond (or (u/boolean? v)
            (integer? v)) (str v)
        (literal? v)      (:value v)
        (keyword? v)      (u/kw->ident v)
        (vector? v)       (expr-vec aliases v)
        :else             (throw (ex-info "Invalid expression" {:got v}))))

;; TODO: unwritten
(defn group-by-vec [])
(defn having-clause [])
(defn windows [])
(defn limit [])
(defn offset [])
(defrecord Param [name])

(defn compile-select [{:keys [from where] :as qry}]
  (let [from (from-item {} (:from qry))]
    ;; where = expr
    ))
(defn compile-insert [qry])
(defn compile-update [qry])
(defn compile-delete [qry])
(defn compile-query [{:keys [select insert update delete] :as qry}])

;; ;; select query
;; {:select
;;  :distinct 
;;  :fields field-list
;;  :from from-items
;;  :where conditions
;;  :group-by ...
;;  :having ...
;;  :windows {:name defn}}
;;  :limit
;;  :offset
;; }

;; {:select :distinct
;;  :fields [:*]
;;  :from [{:t1 :table1}
;;             [:left-join {:t2 :table2} {:on [:= :t1/id :t2/table1_id]}]]
;;  :where [:= :t1/id (? :t1-id)]}
