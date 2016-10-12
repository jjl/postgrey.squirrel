(ns postgrey.sql
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.spec :as s]
            [flatland.ordered.set :as os]
            [postgrey.spec :as ss]
            [postgrey.util :as u :refer [render-string fatal]]))

;;; # postgrey.sql
;;;
;;; This namespace attempts to generate sql from data structures
;;; representing queries. These are low level functions which
;;; are more suitable for machine generation than human usage

;;; The heart of this is the State object, which holds everything
;;; we need to know make the magic work
;;; It stores useful things like placeholders we've seen

;;; ## Diving in
;;;
;;; `> (require '[postgrey.sql :as sql])`

(defmacro some-spec [& ss]
  `(s/or ~@(mapcat (fn [v] [v v]) ss)))
(s/def ::nil nil?)
(s/def ::int integer?)
(s/def ::bool boolean?)
(s/def ::float float?)
(s/def ::string string?)
(s/def ::ident (s/and keyword? #(not= "*" (name %))))
(s/def ::identy (some-fn #(s/valid? ::ident %) symbol?))
(s/def ::ident-atom  (s/and ::ident u/no-ns?))
(derive ::ident-atom ::ident)
(s/def ::binding  u/binding-symbol?)
(s/def ::magic    symbol?)
(s/def ::wild-kw  (s/and keyword? #(= "*" (name %))))
(s/def ::function-name ::ident)
(s/def ::funcall    (s/cat :isa #{'funcall `funcall} :fun ::identy :args (s/* ::expr)))
(s/def ::free       (s/cat :isa #{'free `free} :exprs (s/+ ::expr)))
(s/def ::literal    (s/cat :isa #{'literal `literal} :literal string?))
(s/def ::alias      (s/cat :isa #{'as `as} :name ::ident-atom :expr any?))
(s/def ::verb       #{:select :insert :update :delete :table :values})
(defmulti query-type (fn [v & _] (:verb v)))
(s/def ::query      (s/multi-spec query-type :query/type))
(s/def ::expr       (s/or :int ::int :bool ::bool :float ::float :string ::string :ident ::ident
                          :binding ::binding :magic ::magic :wild-kw ::wild-kw :funcall ::funcall
                          :free ::free :literal ::literal))
(defmethod query-type :select [_]
  (s/keys :req-un [::verb ::select]
          :opt-un [::with ::from ::where ::window ::set-op
                   ::group-by ::order-by ::limit ::for]))

(s/conform ::query {:verb :select
                    :with '[[recursive :foo [:a :b] select {:select [:foo :bar]}]]
                    :select [['as :foo :id] :bar/*] :distinct true
                    :from '[join left
                            [table :foo only as :bar [:baz] sample bernoulli [1.0] seed 4.56]
                            [join cross
                             [with :foo as :bar [:baz]]
                             [join right
                              (select lateral {} as :bar [:baz])
                              (funcall with-ordinality :foo [] as :bar [:baz])
                              [using :a :b]]
                             [using :a :b]]
                            [using :a :b]]
                    :where '[free :a = ?foo] :having '[[free [funcall count :a] > 3]]
                    :window []
                    :set-op '[intersect distinct {:select [:foo :bar]}]
                    :group-by '[[grouping-sets :a [rollup :a :b.c] [cube :d :e]
                                 [grouping-sets [cube :a.b] :c.d]]]
                    :order-by [['[funcall avg :b] 'asc 'nulls-first]
                               ['[funcall stdev :b] 'desc 'nulls-last]]
                    :limit 10 :offset 20
                    :for '[no-key-update of :a :b nowait]})
  
;;; ints: 123, 456
;;; floats: 1.23, 4e5
;;; strings: "", "abc"
;;; identifiers: :foo, :foo/bar
;;; placeholder names: '?foo '?bar
;;; magic symbols: foo bar

(s/def ::bindings (s/coll-of ::binding :kind u/ordered-set?))
(s/def ::state (s/keys :req-un [::bindings]))

(defn new-state
  "Constructs an empty state
   ex: (new-state) ;; => state
   args: []
   returns: state"
  [] {:bindings (os/ordered-set)})

(defn try-place
  "Adds a placeholder to the state, unless it already exists in which case error
   ex: (try-place state :foo) ;; => new-state
   args: [state ph-name]
     ph-name: a non-empty string naming the placeholder
   returns: state"
  [name {:keys [bindings] :as state}]
  (cond (not (u/binding-symbol? name))
        (fatal "Invalid binding" {:got name})

        (not (s/valid? ::state state))
        (fatal "Invalid state" {:got state})

        (contains? bindings name)
        (fatal "Binding already used" {:got name})

       :else
        (update state :bindings conj name)))

;; (s/fdef try-place
;;   :args (s/cat :name ::binding :state ::state)
;;   :ret (s/cat :val string? :state ::state))

;;; `build` is the core multimethod that deals with building up some sql
;;; its first argument is a keyword, on which it is dispatched. it should
;;; be a keyword naming a valid spec (e.g. :postgrey.sql/::expr)

(defmulti build
  "turns data into sql
   ex: (build {::sql/type ::sql/int ::sql/value 123} (sql/new-state))
       ;; => [\"123\" state]
   args: [type data state & args]
     data:  a map with a ::sql/type key
     state: a state map
     args: optional arguments
   dispatch value: type
   returns: [sql state]
     sql: string of sql
     state: a new state"
  (fn [type _ _ & _] type))

;; (s/fdef build
;;   :args (s/cat :spec ::spec :val any? :state ::state :args (s/* any?))
;;   :ret (s/cat :string string? :state ::state))

(defn map-build
  "Builds each item in coll
   args: [spec coll st & args]
   returns: [sqls state]"
  [spec coll st & args]
  (reduce (fn [[acc st] v]
            (let [[r st1] (apply build spec v st args)]
              [(conj acc r) st1]))
          [[] st] coll))

;; (s/fdef map-build
;;   :args (s/cat :spec ::spec :val any? :state ::state :args (s/* any?))
;;   :ret  (s/cat :sql (s/coll-of string?) :state ::state))

(defn join-build
  "Builds each item in coll and joins the result together with sep
   args: [spec coll st sep & args]
   returns: [sql state]"
  [spec coll st sep & args]
  (update (apply map-build spec coll st args) 0
          #(str/join sep %)))

;; (s/fdef join-build
;;   :args (s/cat :spec ::spec :val any? :state ::state :sep string? :args (s/* any?))
;;   :ret  (s/cat :sql ::string :state ::state))

;;; build-some is a utility for automatically building whichever child
;;; matches in a `some-spec`-based spec

(defn build-some
  "Conforms a `some` spec and builds it
   args: [spec val state]
     spec: must be a spec defined by `some-spec`
   returns: [sql state]"
  [spec val st & args]
  (let [[t v2] (ss/conform! spec val)]
    (apply build t v2 st args)))

;; ;; (s/fdef build-some
;; ;;   :args (s/cat :spec ::spec :val any? :state ::state :args (s/* any?))
;; ;;   :ret  (s/cat :sql ::string :state ::state))

;; (defn build-some-coll
;;   "Conforms a collection and then builds each element
;;    args: [spec val state]
;;      spec: must be a `coll-of` spec of an `or` spec
;;    returns: [sqls state]"
;;   [spec val st & args]
;;   (let [v (ss/conform! spec val)]
;;     (reduce (fn [[acc st] [t1 v1]]
;;               (let [[r st2] (apply build t1 v1 st args)]
;;                 [(conj acc r) st2]))
;;             [[] st] v)))

;; (s/fdef build-some-coll
;;   :args (s/cat :spec ::spec :val coll? :state ::state :args (s/* any?))
;;   :ret  (s/cat :sql (s/coll-of ::string) :state ::state))

(defmethod build ::nil     [_ _ st] ["null" st])
(defmethod build ::int     [_ v st] [(str v) st])
(defmethod build ::bool    [_ v st] [(str v) st])
(defmethod build ::float   [_ v st] [(str v) st])
(defmethod build ::string  [_ v st] [(u/render-string v \') st])
(defmethod build ::ident   [_ v st] [(u/render-ident v) st])
(defmethod build ::identy  [_ v st] [(if (symbol? v) (str v) (u/render-ident v)) st])
(defmethod build ::binding [_ v st] ["?" (try-place v st)])
(defmethod build ::magic   [_ v st] [(str v) st])
(defmethod build ::wild-kw [_ v st]
  (let [qualis (->> (-> (or (namespace v) "")
                        (str/split #"\."))
                    (into [] (comp (filter #(not= "" %))
                                   (map #(u/render-string % \"))))
                    (str/join "."))]
    [(if (= "" qualis)
      "*"
      (str qualis ".*"))
     st]))

(defmethod build ::free [_ v st]
  (u/bracket (join-build ::expr (rest v) st " " false)))
  
(defmethod build ::identy [_ v st]
  [(if (symbol? v)
     (str v)
     (u/render-ident v))
   st])

(defmethod build ::funcall [_ v st] 
  (let [{:keys [fun args]} (ss/conform! ::funcall v)
        [fun  st1] (build ::identy fun st)
        [args st2] (join-build ::expr args st1 ", ")]
    [(str fun "(" args ")") st2]))

(defmethod build ::free [_ {:keys [exprs]} st]
  (join-build ::free.item exprs st " "))

(defmethod build ::alias
  [_ {:keys [name expr]} st spec & args]
  (let [[n st1] (build ::ident-atom name st)
        [e st2] (apply build spec expr st1 args)]
    [(str n " as " e) st2]))

(defn expr-type [v]
  (-> [::nil ::int ::bool ::float ::ident ::binding ::funcall ::wild-kw ::magic ::free]
      (ss/some-spec v)))

(defmethod build ::expr [_ v st & [bracket?]]
  (if-let [s (expr-type v)]
    (u/maybe-bracket (build s v st) (if (boolean? bracket?) bracket? false))
    (u/fatal "Expected expression" {:got v :expr-type (expr-type v)})))

(defmulti build-query (fn [v & _] (:verb v)))

(defmethod build ::query [_ v st & [valid]]
  (build-query (ss/conform! ::query v) st))

(s/def ::distinct-on (s/cat :isa #{'on} :exprs (s/+ any?)))

;;; DISTINCT / DISTINCT ON (expr...)
;;; (build ::distinct ['on :a] nil)
;;; (build ::distinct true nil)
(defmethod build ::distinct
  [_ v st]
  (cond (nil? v) ["" st]
        (true? v) ["distinct" st]
        :else (let [r (ss/conform! ::distinct-on v)]
                (update (join-build ::expr (:exprs r) st ", " false) 0
                        #(str "distinct on (" % ")")))))


;;; WHERE expr
;;; (build ::where true nil) 
(defmethod build ::where [_ v st]
  (if (nil? v)
    ["" st]
    (update (build ::expr v st false) 0
            #(str "where " %))))

;;; HAVING expr...
(defmethod build ::having [_ v st]
  (if (nil? v)
    ["" st]
    (update (join-build ::expr v st ", " false) 0
            #(str "having " %))))

;;; GROUP BY
(s/def ::group-by.rollup (s/cat :op '#{rollup} :exprs (s/+ ::expr)))
(s/def ::group-by.cube   (s/cat :op '#{cube}   :exprs (s/+ ::expr)))
(s/def ::group-by.sets   (s/cat :op '#{grouping-sets} :sets (s/+ ::group-by.elem)))
(s/def ::group-by.elem (some-spec ::group-by.rollup ::group-by.cube ::group-by.sets ::expr))
(s/def ::group-by (s/coll-of ::group-by.elem))

(defmethod build ::group-by.rollup [_ {:keys [exprs]} st]
  (update (join-build ::expr exprs st ", " false)
          0 #(str "rollup (" % ")")))

(defmethod build ::group-by.cube [_ {:keys [exprs]} st]
  (update (join-build ::expr exprs st ", " false)
          0 #(str "cube (" % ")")))

(defmethod build ::group-by.sets [_ {:keys [sets]} st]
  (let [sets1 (map #(s/unform ::group-by.elem %) sets)]
    (update (join-build ::group-by.elem sets1 st ", ")
            0 #(str "grouping sets (" % ")"))))

(defmethod build ::group-by.elem
  [_ v st]
  (build-some ::group-by.elem v st))

(defmethod build ::group-by
  [_ v st]
  (if v
    (update (join-build ::group-by.elem v st ", ") 0 #(str "group by " %))
    ["" st]))

;; a field selector. might be an alias
;; debugging output could be better from this - we use expr to error out

(defmethod build ::field
  [_ v st]
  (let [r (s/conform ::alias v)]
    (if (= ::s/invalid r)
      (build ::expr v st)
      (build ::alias r st ::expr false))))
          
;;; Field list, e.g. foo, bar.*
;;; (build ::fields [123 456] nil)

(defmethod build ::fields
  [_ v st]
  (join-build ::field v st ", "))

(defmethod build ::limit
  [_ v st]
  (cond (nil? v) ["" st]
        (not (and (integer? v) (< 0 v)))
        (u/fatal "Limit must be a positive integer" {:got v})
        :else [(str "limit " v) st]))

(defmethod build ::offset
  [_ v st]
  (cond (nil? v) ["" st]
        (not (and (integer? v) (< 0 v)))
        (u/fatal "Offset must be a positive integer" {:got v})
        :else [(str "offset " v) st]))

(s/def ::order (some-fn #{'asc 'desc `asc `desc}
                        #(and (sequential? %) (#{'using `using} (first %)) (symbol? (second %)))))
(s/def ::nulls #{'nulls-first 'nulls-last `nulls-first `nulls-last})
(s/def ::order-by.item (s/cat :expr ::expr :order ::order :nulls (s/? ::nulls)))
(s/def ::order-by (s/coll-of ::order-by.item))

(defmethod build ::order
  [_ v st]
  (if (symbol? v)
    [(name v) st]
    (match v
      (_ :guard symbol?) [(name v) st]
      (['using id] :seq) [(str "using " id) st]
      :else (u/fatal "expected one of 'asc, 'desc, ['using operator]" {:got v}))))

(defmethod build ::order-by.item
  [_ v st]
  (let [{:keys [expr order nulls]} (ss/conform! ::order-by.item v)
        [e st1] (build ::expr  expr  st false)
        o (if (symbol? order) (name order)
              (->> order second name (str "using ")))
        n (if nulls (str/replace (name v) \- \space))]
    [(str/join " " [e o n]) st1]))

(defmethod build ::order-by
  [_ v st]
  (if v
    (update (join-build ::order-by.item v st ", ") 0 #(str "order by " %))
    ["" st]))

(s/def ::for.strength '#{update no-key-update share key-share})
(s/def ::for.wait (some-fn nil? #{'nowait 'skip-locked}))
(s/def ::for.of (s/cat :of #{'of} :idents (s/+ ::ident)))
(s/def ::for (s/cat :strength ::for.strength :of (s/? ::for.of) :wait (s/? ::for.wait)))

(defmethod build ::for
  [_ v st]
  (letfn [(prn-of [of]
            (if (seq (:idents of))
              (str "of " (str/join ", " (map u/render-ident (:idents of))))
              ""))]
    (if (empty? v) ["" st]
      (let [{:keys [strength of wait]} (ss/conform! ::for v)
            strength (u/undashed-name (str strength))
            of (prn-of of)
            wait (if wait (u/undashed-name (str wait)) "")]
        [(str/join " " ["for" strength of wait]) st]))))

(s/def ::set-op (s/cat :op '#{union intersect except}
                       :distinctness '#{all distinct} :query map?))

(defmethod build ::set-op
  [_ v st]
  (if v
    (let [{:keys [op distinctness query]} (ss/conform! ::set-op v)
          distinct (if distinctness (str distinctness " ") "")]
      (update (build ::query ['select query] st) 0
              #(str op " " distinct %)))
    ["" st]))

(s/def ::with.item (s/cat :recursive? (s/? '#{recursive})
                          :name ::ident-atom
                          :args (s/? (s/coll-of ::ident-atom :type vector?))
                          :verb '#{select insert update delete values table}
                          :query map?))

(s/def ::with (s/coll-of ::with.item))

(defmethod build ::with.item
  [_ v st]
  (let [{:keys [recursive? name args verb query]} (ss/conform! ::with.item v)
        rec (if recursive? "recursive " "")
        [args st1] (if (seq args)
                     (u/bracket (join-build ::ident args st ", "))
                     ["" st])
        [q    st2] (build ::query [verb query] st1)]
    [(str rec args " as (" q ")") st2]))

(defmethod build ::with [_ v st]
  (if (seq v)
    (update (join-build ::with.item v st ", ") 0 #(str "with " %))
    ["" st]))

(s/conform ::from '(join left
                         (table :foo only as :bar [:baz] sample bernoulli [1.0] seed 4.56)
                         (join cross
                               (with :foo as :bar [:baz])
                               (join right
                                     (select lateral {} as :bar [:baz])
                                     (funcall with-ordinality :foo [] as :bar [:baz])
                                     [using :a :b])
                               [using :a :b])
                         [using :a :b]))
(s/def ::column-aliases (s/coll-of ::ident-atom :kind vector?))
(s/def ::sample.args    (s/coll-of ::float :kind vector? :min-count 1))
(s/def ::sample.seed    (s/cat :op #{'seed `seed} :val float?))
(s/def ::sample         (s/cat :op #{'sample `sample} :method symbol?
                               :args (s/? ::sample.args) :seed (s/? ::sample.seed)))
(s/def ::from.table     (s/cat :op #{'table `table} :table ::ident :only? (s/? #{'only `only})
                               :as (s/? (s/cat :op #{'as `as} :name ::ident-atom :col-aliases (s/? ::column-aliases)))
                               :sample (s/? ::sample)))
(s/def ::from.with      (s/cat :op #{'with `with} :query ::ident-atom
                               :as (s/? (s/cat :op #{'as `as} :name ::ident-atom  :col-aliases (s/? ::column-aliases)))))
(s/def ::from.select    (s/cat :op #{'select `select} :lateral (s/? #{'lateral `lateral}) :query map?
                               :as (s/? (s/cat :op #{'as `as} :name ::ident-atom :col-aliases (s/? ::column-aliases)))))
(s/def ::from.funcall   (s/cat :op #{'funcall `funcall} :with-ordinality (s/? #{'with-ordinality `with-ordinality})
                               :fun ::identy :args (s/coll-of ::expr :kind vector?)
                               :as (s/? (s/cat :op #{'as `as} :name ::ident-atom :col-aliases (s/? ::column-aliases)))))
(s/def ::from.join.on (s/cat :op #{'on `on} :expr ::expr))
(s/def ::from.join.using (s/cat :op #{'using `using} :cols (s/+ ::ident-atom)))
(s/def ::from.join (s/cat :op #{'join `join} :direction #{'left `left 'right `right 'cross `cross 'full `full}
                          :left ::from :right ::from
                          :cond (s/? (s/or :on ::from.join.on :using ::from.join.using))))
(s/def ::from (s/or :ident ::ident :table ::from.table :with ::from.with :select ::from.select
                    :funcall ::from.funcall :join ::from.join))
;; do these later, they're not used much
;; (s/def ::from.fundef    )
;; variation 5: function_name ( [ argument [, ...] ] ) AS [ alias ] ( column_definition [, ...] )
;;   workings:
;;     from function_name (args...) as alias ( column_def... )
;;       opt: alias
;;         opt: column_definition...
;;   keys:
;;     :func [name & args]
;;     :func/as name ; opt
;;     :cols/defs ; required
;; (s/def ::from.rows-from )
;; variation 6: ROWS FROM( function_name ( [ argument [, ...] ] ) [ AS ( column_definition [, ...] ) ] [, ...] )
;;              [ WITH ORDINALITY ] [ [ AS ] alias ]
;;   workings:
;;     from rows from (LIST)
;;       opt: lateral, with ordinality
;;       opt: as (coldefs)
;;     LIST:
;;       func(args...)+
;;         opt: as (coldef+)
;;   keys:
;;     :rows-from LIST


;;; windows
;;; we don't track enough information to enforce the validity of window clauses
;;; for example, if you define a window with a partition clause, then base a new
;;; one on that existing one and give it a partition clause, it will error
;;; we do however check start and end frame clauses properly

(def simple-frame? '#{current-row all-before all-after})
(defn vector-frame? [v]
  (when (vector? v)
    (let [[a b] v]
      (and (integer? a) (pos? a)
           ('#{before after} b)))))

(def frame? (some-fn simple-frame? vector-frame?))

(s/def ::window.existing    (s/cat :op '#{based-on} :id ::ident))
(s/def ::window.partition   (s/cat :op '#{partition-by} :exprs (s/+ any?)))
(s/def ::window.frame.any   frame?)
(s/def ::window.frame.start (s/and ::window.frame.any #(not= 'all-after  %)))
(s/def ::window.frame.end   (s/and ::window.frame.any #(not= 'all-before %)))
(s/def ::window.frame       (s/cat :op '#{frame} :start ::window.frame.start
                                   :end (s/? ::window.frame.end)))
(s/def ::window.item (s/cat :existing     (s/? ::window.existing)
                            :partition-by (s/? ::window.partition)
                            :order-by     (s/? ::order-by)
                            :frame        (s/? ::window.frame)))

(defmethod build ::window.frame.any [_ v st]
  (cond (symbol? v) (case v
                      current-row ["current row" st]
                      all-before  ["unbounded preceding" st]
                      all-after   ["unbounded following" st]
                      (u/fatal "unrecognised window frame symbol" {:got v}))
        (empty? v) ["" st]
        :else (let [[c p] v]
                (cond (not (integer? c)) (u/fatal "expected integer row count" {:got c})
                      (= 'before p)      [(str c " preceding") st]
                      (= 'after  p)      [(str c " following") st]
                      :else (u/fatal "expected 'before or 'after" {:got p})))))


(defmethod build ::window.frame [_ v st]
  (let [{start 'start end 'end} (ss/conform! ::window.frame v)
        [s st1] (build ::window.frame.any start st)
        [e st2] (build ::window.frame.any end st1)]
    (if (empty? end)
      [(str "rows " s) st2]
      [(str "rows between " s " and " e) st2])))
    
(defmethod build ::window.item [_ v st]
  (let [{:keys [existing partition-by order-by frame]} (ss/conform! ::window.item v)
        e (if existing (u/render-ident (:id existing)) "")
        [p st1] (if (not partition-by) ["" st]
                    (update (join-build ::expr (:exprs partition-by) st ", ") 0 #(str "partition by " %)))
        [o st2] (if order-by (build ::order-by order-by st1) ["" st1])
        [f st3] (build ::window.frame frame st2)]
    [(str/join " " (filter #(not= "" %) [e p o f]))
     st3]))

(defmethod build ::window [_ v st]
  (if (empty? v) ["" st]
    (update (join-build ::window.item v st ", ") 0 #(str "window " %))))

(defmethod build-query 'select
  [{{:keys [with distinct select from where group-by having
            window set-op order-by limit offset]
     for* :for}
    :query :as q} st]
  (let [[with      st1]  (build ::with  with st)
        [distinct  st2]  (build ::distinct distinct st1)
        [fields    st3]  (build ::fields select st2)
        [from      st4]  (build ::from from st3)
        [where     st5]  (build ::where where st3)
        [group-by  st6]  (build ::group-by group-by st5)
        [having    st7]  (build ::having having st6)
        [window    st8]  (build ::window window st7)
        [set-op    st9]  (build ::set-op set-op st8)
        [order-by  st10] (build ::order-by order-by st7)
        [limit     st11] (build ::limit limit st10)
        [offset    st12] (build ::offset offset st11)
        [for*      st13] (build ::for for* st12)
        q-bits (into [] (filter #(not= "" (first %)))
                     [with "select" distinct fields ;; from
                      where group-by having ;;window
                      set-op order-by limit offset for*
                ])]
    [(str/join " " q-bits) st13]))

;; (s/def ::only (s/cat :only #{:only} :id ::id))

;; (s/def ::from.table-item (s/or :alias ::alias :tabular-alias ::tabular-alias :id ::id))
;; (s/def ::from.table (s/or :table-item ::from.table-item
;;                           :only (s/cat :only (s/? #{:only}) :table-item ::from.table-item)))

;; ;; (s/def ::from-1 ...)
;; ;; (s/def ::from-2 ...)
;; ;; (s/def ::from-3 ...)
;; ;; (s/def ::from-4 ...)

  
