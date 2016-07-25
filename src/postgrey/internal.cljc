(ns postgrey.internal)
  ;; (:require [clojure.string :as str]
  ;;           [clojure.core.match :refer [match]]
  ;;           [postgrey.state :as s]
  ;;           [postgrey.util :as u :refer [boolean? s->]])
  ;; (:import [clojure.lang Keyword])
  ;; (:refer-clojure :exclude [cast group-by]))

;; (declare expr exprs from-item field-item)

;; (defn from-on
;;   "Generates the sql for an 'on' clause in a 'from'
;;    args: [state expr]
;;    returns: sql string"
;;   [state on]
;;   (if (nil? on)
;;     [state ""]
;;     (update (expr state on) 1 #(str " on " % ))))

;; (def ^:private join-cond-err
;;   "a join condition must be one of the following forms:
;;     :natural - perform a natural join (auto-'using' all same-named columns)
;;     [:on cond] - conditional join
;;     [:using & cols] - conditional on these cols, which must exist in both tables!
;;     [:lateral expr]")

;; (defn join-cond
;;   "Generates the sql for join conditions
;;    args: [state c]
;;      c must take one of the following forms:
;;        nil / default - unconditional
;;        :natural - natural join tables (on all same-named cols in tables)
;;        [:on cond] - conditional join
;;        [:using col & cols] - join tables on these same-named columns.
;;          every col must be a keyword and there must be at least one col
;;        [:lateral expr] - not yet implemented
;;    returns: [state sql-string]"
;;   [state c]
;;   (letfn [(ut [es] (and (next es) (every? keyword? es)))]
;;     (match [c]
;;       [nil]           [state nil]
;;       [:natural]      [state " natural"]
;;       ;; [[:lateral e]]  (expr state (nth c 1))
;;       [[:on cond]]    (from-on state cond)
;;       [[:using & (ks :guard ut)]] (let [ns (map u/kw->ident (rest c))]
;;                                    [state (str " using (" (str/join "," ns ) ")")])
;;       :else (u/fatal join-cond-err {:got c}))))

;; (defn join
;;   "Returns the sql for a join from a vector describing it
;;    args: [[:join dir t1 t2 & [condition]]]
;;      dir: one of :left :right :inner :full
;;      t1, t2: table expression
;;      condition must be one of:
;;        nil / default - unconditional
;;        :natural - natural join tables (on all same-named cols in tables)
;;        [:on cond] - conditional join
;;        [:using col & cols] - join tables on these same-named columns.
;;          every col must be a keyword and there must be at least one col
;;        [:lateral expr] - not yet implemented
;;    returns: sql string"
;;   [state v]
;;   (let [types #{:left :right :inner :full}]
;;     (match [v]
;;       [([:join type t1 t2 & on] :seq)]
;;       (if (types type)
;;         (let [[as2 [t1_ t2_ on_]] (s-> state (from-item t1) (from-item t2) (from-on (first on)))]
;;           [as2 (str " " t1_ " " (name type) " join " t2_ on_ " ")])
;;         (u/fatal "Invalid join type, must be one of :left :right :inner :full." {:got type}))
;;       :else (u/fatal "Vectors are of the form `[:join :type :table1 :table2 & [on]]" {:got v}))))

;; (defn from-map
;;   "Returns the sql for an aliased from expression
;;    args: [state v]
;;    returns: [state v]"
;;   [state m]
;;   (when-not (seq m)
;;     (u/fatal "Empty map not permitted as from item" {:got m}))
;;   (when-not (every? keyword? (keys m))
;;     (u/fatal "Keys in a from item map must be keywords!" {:got (keys m)}))
;;   (-> (fn [state [k v]]
;;         (update (from-item state v) 1
;;                 #(str " " % " as " (u/kw->ident k) " ")))
;;       (u/skeep state (seq m))
;;       (update 1 #(str/join "," %))))

;; (defn funcall
;;   "Returns a sql funcall expression: `my.function(*args)`
;;    true when expr is not null
;;    args: [args]
;;    returns: sql string"
;;   [state fun args]
;;   (let [n  (u/funkw->ident fun)
;;         [st es] (u/smap expr state args)]
;;     [st (str n "(" (str/join "," es) ")")]))

;; (defn from-item
;;   "Returns the sql for a from expression
;;    args: [state v & [query?]] type-dependent behaviour on v:
;;      keyword: names a table
;;      map: aliases one or more tables
;;      vector: one of:
;;        [:only :table] - ONLY modifier, excludes subtables when using inheritance
;;        [:join direction t1 t2 & [on]] - join clause, see `join`
;;    if query? is true (default false), permits subqueries (not yet implemented)
;;    returns: string"
;;   [state v]
;;   (match [v]
;;     [[:only t]]                [state (str " only " (u/kw->ident t) " ")]
;;     [[:join & is]]             (join state v)
;;     ;; [[:sample & ss]] (throw (ex-info "Sorry, :sample/TABLESAMPLE is not yet implemented" {}))
;;     [(_ :guard literal?)]      [state (:value v)]
;;     [(_ :guard map?)]          (from-map state v)
;;     [(_ :guard u/fnish-kw? v)] (funcall state v [])
;;     :else                      (expr state v)))

;; (defn from [state f]
;;   (cond (nil? f)    [state ""]
;;         (vector? f) (update (u/smap from-item state f) 1
;;                             #(str " from " (str/join "," %)))
;;         :else       (u/fatal ":from must either be nil or a vector" {:got f})))

;; (defn logop
;;   "Returns sql for a logical expression: `expr1 op expr2 ... op exprN`
;;    true when any expression returns true. 0 args = true.
;;    args: [state op args]
;;    returns: [state sql]"
;;   [state op args]
;;   (if (= 0 (count args))
;;     [state "true"]
;;     (update (u/smap expr state args) 1
;;             #(str "(" (str/join (str " " op " ") %) ")"))))

;; (defn postop
;;   "Returns sql for a postfix operator (or operator-like)
;;    args: [state op arg]
;;      op: string, e.g. \"not null\"
;;    returns: [state sql]"
;;   [state op arg]
;;   (update (expr state arg) 1
;;           #(str % " " op)))

;; (defn op
;;   "Returns sql for an operator (or operator-like) application
;;    If one arg, is assumed to be prefix
;;    If multiple args, inserted between all exprs
;;    args: [state op args]
;;    returns: [state sql]"
;;   [state op args]
;;   (let [[st as] (u/smap expr state args)
;;         op2 (u/kw->op op)]
;;     (if (= 1 (count args))
;;       [st (str op2 " " (first as))]
;;       [st (str/join (str " " op2 " ") as)])))

;; (defn expr-kw-vec
;;   ""
;;   [state kw args]
;;   (if (u/fnish-kw? kw)
;;     (funcall state kw args)
;;     (op state kw args)))

;; (defn expr-kw
;;   ""
;;   [state kw args]
;;   (if (u/fnish-kw? kw)
;;     (funcall state kw args)
;;     [state (u/kw->ident kw)]))

;; (defn expr
;;   "Returns the sql for an expression
;;    args: [state expr]
;;    returns: sql string"
;;   [state v]
;;   (let [kw? keyword?]
;;     (match [v]
;;       [(_ :guard u/simple?)]  [state (str v)]
;;       [(_ :guard literal?)]   [state (str (:value v))]
;;       [(_ :guard kw?)]        (expr-kw state v [])
;;       [[:cast e1 e2]]         (let [[st r] (expr state e1)]
;;                                 [st (str "(" r " :: " e2 ")")])
;;       [[:and & args]]         (logop state "and" args)
;;       [[:or & args]]          (logop state "or"  args)
;;       [[:null? e]]            (postop state "is null" e)
;;       [[:not-null? e]]        (postop state "is not null" e)
;;       [[:<> & args]]          (op state :<> args)
;;       [[(k :guard kw?) & as]] (expr-kw-vec state k as)
;;       :else                   (u/fatal "Invalid expression" {:got v}))))

;; (defn exprs
;;   ""
;;   [state es]
;;   (update (u/smap expr state es) 1
;;           #(str "(" (str/join "," %) ")")))

;; (defn where
;;   ""
;;   [state condition]
;;   (if-not (nil? condition)
;;     (update (expr state condition) 1
;;             #(str "where " %)))
;;   [state ""])

;; (declare grouping-elements)

;; (defn grouping-element [state e]
;;   (match [e]
;;     [[:rollup & es]]          (update (exprs state es) 1
;;                                       #(str "rollup (" % ")"))
;;     [[:cube & es]]            (update (exprs state es) 1
;;                                       #(str "cube (" % ")"))
;;     [[:grouping-sets & ges]]  (update (grouping-elements state ges) 1
;;                                       #(str "grouping sets (" % ")"))
;;     [(_ :guard vector?)]      (exprs state e)))

;; (defn grouping-elements [state ges]
;;   (update (u/smap grouping-element state ges) 1
;;           (partial str/join ",")))

;; (defn group-by [state es]
;;   (update (grouping-elements state es)
;;           #(str " group by " %)))
;; (defn having
;;   [state condition]
;;   (if-not (nil? condition)
;;     (update (expr state condition) 1
;;             #(str " having " %)))
;;   [state ""])

;; ;; ;; ORDER BY expression [ ASC | DESC | USING operator ] [ NULLS { FIRST | LAST } ] [, ...] ]
;; ;; (def ^:private order-error
;; ;;   ":order-by accepts on or more vectors of the form:
;; ;;     [e sort nulls]
;; ;;       e:     expression
;; ;;       sort:  :asc, :desc or [:using op] where op is a keyword or literal
;; ;;       nulls: :first or :last, where to place nulls in the results")

;; ;; (defn order [state es]
;; ;;   (-> (fn [st es]
;; ;;         (match [es]
;; ;;           [[e sort & nulls]]
;; ;;           (let [s (match [sort]
;; ;;                     [(:or :asc :desc)] (name sort)
;; ;;                     [[:using op]] (str "using " (name op))
;; ;;                     :else (u/fatal order-error {:got es}))
;; ;;                 n (case (nth nulls 1 :nulls-last)
;; ;;                     :nulls-first "nulls first"
;; ;;                     :nulls-last  "nulls last"
;; ;;                     (u/fatal order-error {:got es}))]
;; ;;             (update (expr state e) 1
;; ;;                     #(str % " " s " " n)))

;; ;;                :else (u/fatal order-error {:got es})))
;; ;;       (u/smap state es)
;; ;;       (update 1 (partial str " order by "))))

;; (defn limit
;;   "Returns sql for a limit clause
;;    args: [state count]
;;      count: an integer
;;    returns: [state sql]"
;;   [state count]
;;   (when-not (integer? count)
;;     (u/fatal "Expected integer in limit" {:got count}))
;;   [state (str "limit " count)])

;; (defn offset
;;   "Returns sql for an offset clause
;;    args: [state count]
;;      count: an integer
;;    returns: [state sql]"
;;   [state count]
;;   (when-not (integer? count)
;;     (u/fatal "Expected integer in limit" {:got count}))
;;   [state (str "offset " count)])

;; (defn for-nulls [state args]
;;   [state
;;    (-> #{:nulls-first :nulls-last}
;;        (filter args)
;;        first
;;        ({:nulls-first " nulls first" :nulls-last " nulls last"} " nulls last"))])

;; (defn for-of [state args]
;;   (let [of (concat (filter vector? args))]
;;     (when-not of
;;       (u/fatal "Vector arguments to `for` (i.e. `of`) must contain keywords" {:got []}))
;;     (when-not (every? keyword? of)
;;       (u/fatal "Vector arguments to `for` (i.e. `of`) must consist of keywords only" {:got of}))
;;     [state (str/join "," (mapv u/kw->ident of))]))
       
;; (defn for-lock [state args]
;;   [state
;;    (-> #{:no-wait :skip-locked}
;;        (filter args)
;;        first
;;        ({:no-wait " nowait" :skip-locked " skip locked"} ""))])

;; (defn for-clause
;;   ""
;;   [state args]
;;   (let [tmap {:update "update" :no-key-update "no key update"
;;               :share "share" :key-share "key share"}
;;         [t & rs] args
;;         type       (tmap t)
;;         nulls (for-nulls state rs)
;;         lock  (for-lock state rs)]
;;     [state (str "for " type nulls lock)]))

;; (defn basic-field-item
;;   "A basic field item is a keyword, a Literal or an expression
;;    args: [state v]
;;    returns: [state sql]"
;;   [state v]
;;   (match [v]
;;     [(_ :guard literal?)]      [state (:value v)]
;;     [(_ :guard u/fnish-kw? v)] (funcall state v [])
;;     :else                      (expr state v)))
  
;; (defn field-map
;;   "Returns the sql for an aliased from expression
;;    args: [state v]
;;    returns: [state v]"
;;   [state m]
;;   (when-not (seq m)
;;     (u/fatal "Empty map not permitted as field item" {:got m}))
;;   (when-not (every? keyword? (keys m))
;;     (u/fatal "Keys in a field item map must be keywords!" {:got (keys m)}))
;;   (-> (fn [state [k v]]
;;         (update (field-item state v) 1
;;                 #(str " " % " as " (u/kw->ident k) " ")))
;;       (u/skeep state (seq m))
;;       (update 1 #(str/join "," %))))

;; (defn field-item
;;   "A field item is a basic field item or a map"
;;   [state v]
;;   (if (map? v)
;;     (field-map state v)
;;     (basic-field-item state v)))

;; (defn fields [state fs]
;;   (when-not (and (vector? fs) (seq fs))
;;     (u/fatal "Field-items must be a vector. It is not optional." {:got fs}))
;;   (update (u/smap field-item state fs) 1
;;           #(str "(" (str/join "," %) ")")))

;; (defrecord Param [name])

;; (defn select
;;   "Returns the initial part of the select, i.e. the select itself
;;    args: [state s]
;;    returns: [state s]"
;;   [state s]
;;   (match [s]
;;     [:all]      "select"
;;     [:distinct] "select distinct"
;;     [[:distinct-on & es]] (update (exprs state es) 1
;;                                   #(str "select distinct on (" % ")")))
;;     :else  (u/fatal "a select clause must be one of the following forms:
;;     :all - an ordinary (i.e. non-distinct) query
;;     :distinct - select distinct
;;     [:distinct-on & exprs]" {:got s}))

;; (defn compile-select [qry]
;;   (s-> s/empty-state
;;        (select   (:select   qry))
;;        (fields   (:fields   qry))
;;        (from     (:from     qry))
;;        (where    (:where    qry))
;;        (group-by (:group-by qry))
;;        (having   (:having   qry))))
         ;; (window   (:window qry))
         ;; (union    (:union qry))
         ;; (except   (:except qry))
         ;; (order-by (:order-by qry))))
  ;; (let [[state1]  (select-clause empty-state select)
  ;;       [sql2 fields]  (fields state1
          

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
