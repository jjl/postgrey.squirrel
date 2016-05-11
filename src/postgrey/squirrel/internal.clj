(ns postgrey.squirrel.internal
  (:require [clojure.string :as str]
            [clojure.core.match :refer [match]]
            [postgrey.squirrel.state :as s]
            [postgrey.squirrel.util :as u :refer [boolean? s->]])
  (:import [clojure.lang Keyword])
  (:refer-clojure :exclude [cast]))

(defprotocol Compile
  (compile [self params]
    "Renders a query into a format suitable for passing to jdbc
     args: [self params]
       params is a map of bindings to values
     returns: [sql bindings-vec]"))

(defrecord Literal [value])
(defrecord Placeholder [name])

(defrecord Query [sql params presets]
  Compile
  (compile [q ps]
    (let [ps' (merge presets ps)
          pv (mapv #(or (ps' %)
                        (u/fatal "Missing parameter:" {:name % :got ps :presets presets}))
                   params)]
      [sql pv])))

(declare expr exprs from-item)

(defn literal?
  "True if passed an instance of Literal
   args: [l]
   returns: bool"
  [l]
  (instance? Literal l))

(defn from-on
  "Generates the sql for an 'on' clause in a 'from'
   args: [state expr]
   returns: sql string"
  [state on]
  (if (nil? on)
    [state ""]
    (update (expr state on) 1 #(str " on " % ))))

(def ^:private join-cond-err
  "a join condition must be one of the following forms:
    :natural - perform a natural join (auto-'using' all same-named columns)
    [:on cond] - conditional join
    [:using & cols] - conditional on these cols, which must exist in both tables!
    [:lateral expr]")

(defn join-cond
  "Generates the sql for join conditions
   args: [state c]
     c must take one of the following forms:
       nil / default - unconditional
       :natural - natural join tables (on all same-named cols in tables)
       [:on cond] - conditional join
       [:using col & cols] - join tables on these same-named columns.
         every col must be a keyword and there must be at least one col
       [:lateral expr] - not yet implemented
   returns: [state sql-string]"
  [state c]
  (letfn [(ut [es] (and (next es) (every? keyword? es)))]
    (match [c]
      [nil]           [state nil]
      [:natural]      [state " natural"]
      ;; [[:lateral e]]  (expr state (nth c 1))
      [[:on cond]]    (from-on state cond)
      [[:using & (ks :guard ut)]] (let [ns (map u/kw->ident (rest c))]
                                   [state (str " using (" (str/join "," ns ) ")")])
      :else (u/fatal join-cond-err {:got c}))))

(defn join
  "Returns the sql for a join from a vector describing it
   args: [[:join dir t1 t2 & [condition]]]
     dir: one of :left :right :inner :full
     t1, t2: table expression
     condition must be one of:
       nil / default - unconditional
       :natural - natural join tables (on all same-named cols in tables)
       [:on cond] - conditional join
       [:using col & cols] - join tables on these same-named columns.
         every col must be a keyword and there must be at least one col
       [:lateral expr] - not yet implemented
   returns: sql string"
  [state v]
  (let [types #{:left :right :inner :full}]
    (match [v]
      [([:join type t1 t2 & on] :seq)]
      (if (types type)
        (let [[as2 [t1_ t2_ on_]] (s-> state (from-item t1) (from-item t2) (from-on (first on)))]
          [as2 (str " " t1_ " " (name type) " join " t2_ on_ " ")])
        (u/fatal "Invalid join type, must be one of :left :right :inner :full." {:got type}))
      :else (u/fatal "Vectors are of the form `[:join :type :table1 :table2 & [on]]" {:got v}))))

(defn from-map
  "Returns the sql for an aliased from expression
   args: [state v]
   returns: [state v]"
  [state m]
  (when-not (seq m)
    (u/fatal "Empty map not permitted as from item" {:got m}))
  (when-not (every? keyword? (keys m))
    (u/fatal "Keys in a from item map must be keywords!" {:got (keys m)}))
  (-> (fn [state [k v]]
        (update (from-item state v) 1
                #(str " " % " as " (u/kw->ident k) " ")))
      (u/skeep state (seq m))
      (update 1 #(str/join "," %))))

(defn funcall
  "Returns a sql funcall expression: `my.function(*args)`
   true when expr is not null
   args: [args]
   returns: sql string"
  [state fun args]
  (let [n  (u/funkw->ident fun)
        [st es] (u/smap expr state args)]
    [st (str n "(" (str/join "," es) ")")]))

(defn from-item
  "Returns the sql for a from expression
   args: [state v & [query?]] type-dependent behaviour on v:
     keyword: names a table
     map: aliases one or more tables
     vector: one of:
       [:only :table] - ONLY modifier, excludes subtables when using inheritance
       [:join direction t1 t2 & [on]] - join clause, see `join`
   if query? is true (default false), permits subqueries (not yet implemented)
   returns: string"
  [state v]
  (match [v]
    [[:only t]]                [state (str " only " (u/kw->ident t) " ")]
    [[:join & is]]             (join state v)
    ;; [[:sample & ss]] (throw (ex-info "Sorry, :sample/TABLESAMPLE is not yet implemented" {}))
    [(_ :guard literal?)]      [state (:value v)]
    [(_ :guard map?)]          (from-map state v)
    [(_ :guard u/fnish-kw? v)] (funcall state v [])
    :else                      (expr state v)))

(defn logop
  "Returns sql for a logical expression: `expr1 op expr2 ... op exprN`
   true when any expression returns true. 0 args = true.
   args: [state op args]
   returns: [state sql]"
  [state op args]
  (if (= 0 (count args))
    [state "true"]
    (update (u/smap expr state args) 1
            #(str "(" (str/join (str " " op " ") %) ")"))))

(defn postop
  "Returns sql for a postfix operator (or operator-like)
   args: [state op arg]
     op: string, e.g. \"not null\"
   returns: [state sql]"
  [state op arg]
  (update (expr state arg) 1
          #(str % " " op)))

(defn op
  "Returns sql for an operator (or operator-like) application
   If one arg, is assumed to be prefix
   If multiple args, inserted between all exprs
   args: [state op args]
   returns: [state sql]"
  [state op args]
  (let [[st as] (u/smap expr state args)
        op2 (u/kw->op op)]
    (if (= 1 (count args))
      [st (str op2 " " (first as))]
      [st (str/join (str " " op2 " ") as)])))

(defn expr-kw-vec
  ""
  [state kw args]
  (if (u/fnish-kw? kw)
    (funcall state kw args)
    (op state kw args)))

(defn expr-kw
  ""
  [state kw args]
  (if (u/fnish-kw? kw)
    (funcall state kw args)
    [state (u/kw->ident kw)]))

(defn expr
  "Returns the sql for an expression
   args: [state expr]
   returns: sql string"
  [state v]
  (let [kw? keyword?]
    (match [v]
      [(_ :guard u/simple?)]  [state (str v)]
      [(_ :guard literal?)]   [state (str (:value v))]
      [(_ :guard kw?)]        (expr-kw state v [])
      [[:cast e1 e2]]         (let [[st r] (expr state e1)]
                                [st (str "(" r " :: " e2 ")")])
      [[:and & args]]         (logop state "and" args)
      [[:or & args]]          (logop state "or"  args)
      [[:null? e]]            (postop state "is null" e)
      [[:not-null? e]]        (postop state "is not null" e)
      [[:<> & args]]          (op state :<> args)
      [[(k :guard kw?) & as]] (expr-kw-vec state k as)
      :else                   (u/fatal "Invalid expression" {:got v}))))

(defn exprs
  ""
  [state es]
  (update (u/smap expr state es) 1
          #(str " " (str/join "," %) " ")))

(defn group-by-vec [])
(defn having-clause [])

(defn limit
  ""
  [state count]
  [state (str "limit " count)])

(defn offset
  ""
  [state count]
  [state (str "offset " count)])

(defrecord Param [name])

;; (defn select-expr
;;   [state e]
;;   (if (map? e)
;;     ...
;;     (expr state e)))
              
;; (defn select-clause
;;   "Returns the initial part of the select, i.e. the select itself
;;    args: [state s]
;;    returns: [state s]"
;;   [state s]
;;   (case s
;;     :all      "select"
;;     :distinct "select distinct"
;;     (if (and (vector? s)
;;              (= :distinct-on (first s))
;;              (> (count s) 1))
;;       (str "select distinct on (" (exprs state (rest s)) ")")
;;       (throw (ex-info "a select clause must be one of the following forms:
;;     :all - an ordinary (i.e. non-distinct) query
;;     :distinct - select distinct
;;     [:distinct-on & exprs]"
;;                       {:got s})))))

;; (defn compile-select [{:keys [select from where group-by having window
;;                               union except order-by limit offset fetch for] :as qry}]
;;   (let [[sql1 state1]  (select-clause empty-state select)
;;         (if from
;;           (let [[sql2 state2]
          

;; (defn compile-insert [qry])
;; (defn compile-update [qry])
;; (defn compile-delete [qry])
;; (defn compile-query [{:keys [select insert update delete] :as qry}])

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
