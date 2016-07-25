(ns postgrey.sql
  (:require [clojure.string :as str]
            [postgrey.state :as st
             :refer [append prefix comma-sep dot-sep parens
                     hold-place hold-alias error]]
            [postgrey.util :as u]))


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
(count reserved-keywords)

;; A literal's :val is a string and will be inserted without question provided it isn't empty
(defrecord Literal [val])
;; A placeholder has a name and renders as ?
(defrecord Placeholder [name])

(def literal?
  "true if arg is a Literal
   args: [l]
   returns: bool"
  (partial instance? Literal))

(def lit
  "Manufactures a literal (will avoid escaping)
   args: [name]
   returns: Literal"
  ->Literal)

(def hold
  ->Placeholder)
(def ? hold)
(def placeholder?
  (partial instance? Placeholder))

;; (declare item-expr item-as) ;query-select item-keyword item-as)

(defn a-simple
  ([i st]     (a-simple ::a-simple i st))
  ([src i st]
   (let [r (str i)]
     (if (= "" r)
       (append st r)
       (error st {:error "Invalid simple item" :item i :source src})))))

;; (defn a-literal
;;   ([l st]     (a-literal ::item-literal l st))
;;   ([src l st]
;;    (let [r (:val st)]
;;      (if (= "" r)
;;        (error st {:error "Empty literal" :lit r :source src})
;;        (append st r)))))

;; (defn a-keyword-qualified
;;   ([k st] (a-keyword-qualified ::a-keyword-qualified k st))
;;   ([src k st]
;;    (if-let [r (u/sql-repr-qualified ^clojure.lang.Keyword k)]
;;      (append st r)
;;      (error st {:error "Invalid qualified keyword"
;;                 :source src}))))

;; (defn a-keyword-wildcard
;;   ([k st] (a-keyword-wildcard ::a-keyword-wildcard k st))
;;   ([src k st]
;;    (if-let [r (u/sql-repr-qualified-wildcard ^clojure.lang.Keyword k)]
;;      (append st r)
;;      (error st {:error "Invalid qualified keyword"
;;                 :source src}))))

;; (defn a-identifier-string
;;   ([s st] (a-identifier-string ::a-identifier-string k st))
;;   ([src s st]
;;    (if-let [r (u/sql-repr-ident-atom s)]
;;      (append st r)
;;      (error st {:error "Invalid identifier string" :ident s :source src}))))

;; (defn a-symbol
;;   ([s st] (a-symbol ::a-symbol s st))
;;   ([src s st]
;;    (if-let [repr (u/sql-repr-symbol)]
;;      (append st repr)
;;      (error st {:error "invalid symbol"  :valid #"^[a-z_]+"
;;                 :source src  :symbol s}))))

;; (defn a-keyword-unqualified
;;   ([k st] (a-keyword-unqualified ::a-keyword-unqualified k st))
;;   ([src k st]
;;    (if (namespace ^clojure.lang.Keyword k)
;;      (error st {:error "unexpected namespace in keyword" :source src  :keyword k})
;;      (->> k name u/sql-repr-ident-atom (append st)))))

;; (defn a-identifier-atomic
;;   ([i st] (a-identifier-atomic ::a-identifier-atomic i st))
;;   ([src i st]
;;    (cond (string? i)  (a-identifier-string i st)
;;          (literal? i) (a-literal i st)
;;          (symbol? i)  (a-symbol src i st)
;;          (keyword? i) (a-keyword-unqualified src i st)
;;          :else (error st {:error "Invalid identifier atom"
;;                           :valid [:keyword :string :Literal]
;;                           :source src  :identifier i}))))

;; (defn a-identifier-vector
;;   ([v st] (a-identifier-vector ::a-identifier-vector v st))
;;   ([src v st] (dot-sep a-identifier-atomic v st)))

;; (defn a-identifier
;;   ([i st] (a-identifier ::a-identifier i st))
;;   ([src i st]
;;    (cond (literal? i) (a-literal src i st)
;;          (string? i)  (a-identifier-string src i st)
;;          (keyword? i) (a-keyword-qualified src i st)
;;          (vector? i)  (a-identifier-vector src i st)
;;          :else (error st {:error "Invalid identifier"
;;                           :valid [:keyword :string :vector :Literal]
;;                           :source src :identifier i}))))

;; (defn a-as
;;   ([compiler a thing st] (a-as compiler a thing st))
;;   ([src compiler a thing st]
;;    (if-let [v (a-identifier-atomic a)]
;;      (suffix ["as" v] compiler thing (hold-alias st a))
;;      (error st {:error "Invalid alias" :alias a
;;                 :valid [:keyword :string :Literal]
;;                 :source src}))))

;; (defn a-col-def
;;   ([f st] (a-col-def ::a-col-def f st))
;;   ([src f st]
;;    (error st {:error "a-col-def is unimplemented"  :source src})))

;; (defn a-expr-call
;;   ([fun args st] (a-expr-call ::a-expr-call fun args st))
;;   ([src fun args st]
;;    (->> st
;;         (a-identifier fun)
;;         (parens #(comma-sep expr % %2) args))))
       
;; (defn a-expr-infix-op
;;   ([op args] (a-expr-infix-op ::a-expr-infix-op op args))
;;   ([src op args] ...))

;; (defn a-expr-list
;;   ([l st] (a-expr-list ::a-expr-list l st))
;;   ([src l st]
;;    (match l
;;      (['call fun] :seq)      (a-expr-call fun []   st)
;;      (['call fun args] :seq) (a-expr-call fun args st)
;;      (['infix op args] :seq) (a-expr-infix-op op args st)
;;      (['string str] :seq)    (append st (u/sql-repr-string str))
;;      :else (error st {:error "unknown list expression"
;;                       :valid '[call infix]  :list l}))))

;; (defn a-expr
;;   ([e st] (a-expr ::a-expr e st))
;;   ([src e st]
;;     (cond (number? e)  (a-simple            rc e st)
;;           (bool? e)    (a-simple            rc e st)
;;           (literal? e) (a-literal           rc e st)
;;           (list? e)    (a-expr-list         rc e st)
;;           (symbol? e)  (a-symbol            rc e st)
;;           (string?  e) (a-identifier-string rc e st)
;;           (keyword? e) (a-keyword-qualified src e st))))
  
;; (defn a-sub-select
;;   ([q st] (a-sub-select ::a-sub-select q st))
;;   ([src q st]
;;    (->> (without-aliases st)
;;         (parens query-select q)
;;         (replace-aliases st))))
  
;; (defn a-table-sample [s st])

;; (defn a-from-funcall-with-col-defs [f st])
;; (defn a-from-funcall-simple [f st])

;; ;; foo(arg...) with ordinality as alias (column_alias...) ;; 4
;; ;; foo(arg...) with ordinality as alias ;; 4
;; ;; foo(arg...) with ordinality ; 4
;; ;; foo(arg...) as alias (column_alias...) ;; 4
;; ;; foo(arg...) as alias ;; 4
;; ;; foo(arg...) ; 4
;; (defn a-from-funcall [f st]
;;   (let [[fun args opts] f
;;         {:keys [with-ordinality? as cols/defs]} (or opts {})]
;;     (append st 
;;         "("
    
;;     ...)))


;; (defn a-rows-from [f st])
;; ;; {:select [:pg/current_timestamp]}
;; ;; {:select [:f/*]
;; ;;  :from [{:f :foo}]}
;; ;; {:select '(count [] {:with-ordinality? true
;; ;;                      :as :bar :cols/as [...]})
;; ;;  :from [{:f :foo}]}
;; ;; {:select '(count {:with-ordinality? true


;; ;;;;;
;; ;; rows-from makes functions complicated
;; ;; foo(arg...) as bar (col_def...) ; 5
;; ;; foo(arg...) as (col_def...) ; 5
;; ;; rows from ( foo(arg...) as (col_def...) ... ) with ordinality as bar; 6 
;; ;; rows from ( foo(arg...) ... ) with ordinality as bar; 6
;; ;; rows from ( foo(arg...) as (col_def...) ... ) with ordinality; 6 
;; ;; rows from ( foo(arg...) ... ) with ordinality; 6
;; ;; rows from ( foo(arg...) as (col_def...) ... ) as bar; 6 
;; ;; rows from ( foo(arg...) ... ) as bar; 6
;; ;; rows from ( foo(arg...) as (col_def...) ... ) ; 6 
;; ;; rows from ( foo(arg...) ... ) ; 6
;; ;;
;; ;;

;; (defn a.from.funcall.returns-record [f st])

;; ;; TODO: i do not work at all, replace me
;; (defn a.from.rows-from [i st]
;;   (match i
;;     ([:pg/rows-from & rfs] :seq)
;;     (do (append st "rows from" "(")
;;         (reduce #(a.from.funcall %2 %) st rfs)
;;          (fn [st2 v]
;;            (cond (list? v) (a.expr.funcall v st2)
;;                  (map? v)  (match v
;;                                   {:cols/defs ds :funcall f} (->> (a.expr.funcall f st2))
                                  
;;                                   {:funcall f} (a.expr.funcall f st2)
;;                              :else (append)
;;                  :else (error st2 {:error "invalid rows-from clause"
;;                                   :valid [:list]  :got i
;;                                   :source ::a.from.rows-from}))))
;;             st rfs)
;;     :else (error st {:error "invalid rows-from clause"
;;                      :valid [:list]
;;                      :got i
;;                      :source ::a.from.rows-from})
;;   (append st ")")))

;; (defn a.from.sub-query [i st]
;;   (let [{:keys [query lateral?]} i
;;         qa (:query/as i)
;;         ca (:cols/as i)]
;;     (if (and ca (not qa))
;;       (error st {:error "use of :cols/as without :query/as"
;;                  :source ::a.from.sub-query
;;                  :query/as qa :cols/as ca :query i})
;;       (let [st2 (maybe-prefix lateral? "lateral" a.sub-select query st)]))))
        
              
;; (defn a.from.with-query [i st])
;; (defn a.from.func [i st])
;; (defn a.from.join [i st])

;; (defn a.from.funcall
;;   ([f st] (a.from.funcall ::a.from.funcall f st))
;;   ([src f st]))

;; (defn a.from.list
;;   ([l st] (a.from.list ::a.from.list l st))
;;   ([src l st]
;;    (let [disp {'join      a.from.join
;;                'with      a.from.with-query
;;                'query     a.from.sub-query
;;                'values    a.from.values
;;                'call      a.from.funcall
;;                'rows-from a.from.rows-from}]
;;      (cond (not (list? i)) (a.identifier i st)
;;            (not (seq i))   (error st {:error "Empty list"
;;                                       :source ::a.from})
;;            :else (if-let [d (disp (first i))]
;;                    (d (next i))
;;                    (a.from.func i st))))))

;; (defn a.from [i st]
;;   (if (map? i)
;;     (reduce-kv #(alias a.from.helper %2 %3 %) st i)
;;     (a.from i st)))


;; (defn clause.with [name {:keys [query recursive? cols] :as q} state]
;;   (u/ensure-kw name "with query name")
;;   (let [b (:builder state)]
;;     (-append b (if recursive? " with recursive " " with ") name " ")
;;     (when (seq cols)
;;       (parens comma-sep cols state))
;;     (-append b " as ( ")
;;     (let [st2 (compile-select query state)]
;;       (-append b " ) ")
;;       st2)))

;; (defn clause.select [q state]
;;   (match [(:select q) (:distinct-on q)]
;;     [:all nil]      (append state " select ")
;;     [:distinct nil] (append state " select distinct ")
;;     [:distinct es]  (prefix " select distinct on "
;;                             #(parens comma-sep % %2)
;;                              exprs state)
;;     :else (fatal ":select must be either :all or :distinct. :distinct-on may be optionally supplied if :distinct" {:got (:select q)})))

;; (defn a.field.map
;;   ([m st] (a.field.map ::a.field.map m st))
;;   ([src m st]
;;    (->> fs (into (sorted-map)) (reduce-kv #(a.alias expr %2 %3 %) st))))

;; (defn a.field
;;   "args: [field state]
;;     field is one of:
;;       a keyword - an identifier, magic on namespaced and name=* keywords
;;       a map - identifiers (keys) that alias expressions (vals)
;;       an expression - use this expression, generate a name (see `a.expr`)
;;     returns: state"
;;   ([f st] (a.field ::a.field f st))
;;   ([src f st]
;;    (cond (keyword? f) (a.keyword.wildcard f st)
;;          (map? f)     
;;          :else        (a.expr f st))))

;; (defn clauses.fields
;;   "args: [fields state]
;;     fields is one of:
;;       :* - the wildcard selector
;;       a map - to alias expressions
;;         - if you provide us a non-sorted, non-ordered map
;;           we will sort it to behave more predictably
;;       a vector of :*, maps and expressions
;;    returns: state"
;;   ([fs st] (clauses.fields ::clauses.fields fs st))
;;   ([src fs st]
;;    (if (vector? fs)
;;      (reduce #(a.field %2 %1) state fs)
;;      (a.field fs st))))

;; (defn clauses.from [fs state]
;;   (prefix " from " (partial comma-sep a-from) fs state))

;; (defn clauses.where [w state]
;;   (expr c (append state " where ")))

