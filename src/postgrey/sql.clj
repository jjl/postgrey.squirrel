(ns postgrey.sql
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.spec :as s]
            [clojure.spec.gen :as g]
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
;; (generate ::select-query
;;           '{:verb :select
;;             :with [[recursive :foo [:a :b] {:verb :select :select [:foo :bar]}]]
;;             :select [[as :foo :id] :bar/*] :distinct distinct
;;             :from [join left
;;                     [table :foo only as :bar [:baz] sample bernoulli [1.0] seed 4.56]
;;                     [join cross
;;                       [with :foo as :bar [:baz]]
;;                       [join right
;;                         [select lateral {:verb :select :select [123]} as :bar [:baz]]
;;                         (funcall with-ordinality :foo [] as :bar [:baz])
;;                         [using :a :b]]
;;                       [using :a :b]]
;;                     [using :a :b]]
;;             :where [free :a = ?foo] :having [[free [funcall count :a] > 3]]
;;             :window [[frame unbounded-preceding unbounded-following]]
;;             :set-op [intersect distinct {:verb :select :select [:foo :bar]}]
;;             :group-by [[grouping-sets :a [rollup :a :b.c] [cube :d :e]
;;                         [grouping-sets [cube :a.b] :c.d]]]
;;             :order-by [[[funcall avg :b] asc nulls-first]
;;                        [[funcall stdev :b] desc nulls-last]]
;;             :limit 10 :offset 20
;;             :for [no-key-update of :a :b nowait]}
;;           (new-state))
;; (generate ::select '[[as :foo :id] :bar/*] [])
;; (generate ::with.item '[recursive :foo [:a :b] {:verb :select :select [:foo :bar]}] [])

;; (generate ::from '[join left
;;                     [table :foo only as :bar [:baz] sample bernoulli [1.0] seed 4.56]
;;                     [join cross
;;                       [with :foo as :bar [:baz]]
;;                       [join right
;;                         [select lateral {:verb :select :select [123]} as :bar [:baz]]
;;                         (funcall with-ordinality :foo [] as :bar [:baz])
;;                         [using :a :b]]
;;                       [using :a :b]]
;;                     [using :a :b]] [])

;; (clojure.pprint/pprint (s/exercise ::select-query))
;; (s/exercise ::select-query)
(defmacro named [& names]
  (when-not (and (seq names)
                 (every symbol? names))
    (throw (ex-info "named expects symbols as arguments" {:got names})))
  (into #{} (mapcat (fn [v] `['~v '~(symbol "postgrey.sql" (name v))])) names))

(defmacro some-spec [& ss]
  (when-not (and (seq names)
                 (every keyword? names))
    (throw (ex-info "some-spec expects symbols as arguments" {:got names})))
  `(s/or ~@(mapcat (fn [v] [v v]) ss)))

;;; needed for state

(s/def ::binding  (s/with-gen (s/and symbol? u/binding-symbol?)
                    #(g/fmap u/make-binding-symbol (g/symbol))))

;;; The state object simply contains a collection of bindings
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

;;; `build` is the core multimethod that deals with building up some sql
;;; its first argument is a keyword, on which it is dispatched. it should
;;; be a keyword naming a valid spec (e.g. :postgrey.sql/::expr)

(defmulti build
  "turns data into sql
   ex: (build ::sql/int 123 (sql/new-state)) ;; => [\"123\" state]
   args: [type data state]
     data:  a map with a ::sql/type key
     state: a state map
   dispatch value: type
   returns: [sql state]
     sql: string of sql
     state: a new state"
  (fn [type _ _] type))
(defmethod build ::binding [_ v st] ["?" (try-place v st)])

(defn map-build
  "Builds each item in coll
   args: [spec coll st & args]
   returns: [sqls state]"
  [spec coll st]
  (reduce (fn [[acc st] v]
            (let [[r st1] (build spec v st)]
              [(conj acc r) st1]))
          [[] st] coll))

(defn join-build
  "Builds each item in coll and joins the result together with sep
   args: [spec coll st sep & args]
   returns: [sql state]"
  [spec coll st sep]
  (update (map-build spec coll st) 0
          #(str/join sep %)))

(defn build-some [[k v] st]
  (build k v st))

(defn map-some [coll st]
  (reduce (fn [[acc st] v]
            (let [[r st1] (build-some v st)]
              [(conj acc r) st1]))
          [[] st] coll))

(defn join-some [coll st sep]
  (update (map-some coll st) 0
          #(str/join sep %)))

(defn make-join-build [sep]
  (fn [spec coll st]
    (join-build spec coll st sep)))

(defn make-join-some [sep]
  (fn [coll st]
    (join-some coll st sep)))

(def comma-build (make-join-build ","))
(def space-build (make-join-build " "))
(def paren-comma-build (comp u/bracket comma-build))
(def paren-space-build (comp u/bracket space-build))
(def comma-some (make-join-some ","))
(def space-some (make-join-some " "))
(def paren-comma-some (comp u/bracket comma-some))
(def paren-space-some (comp u/bracket space-some))

;;; simple data

(s/def ::nil nil?)
(s/def ::int integer?)
(s/def ::bool boolean?)
(s/def ::float float?)
(s/def ::string string?)
(defmethod build ::nil     [_ _ st] ["null" st])
(defmethod build ::int     [_ v st] [(str v) st])
(defmethod build ::bool    [_ v st] [(str v) st])
(defmethod build ::float   [_ v st] [(str v) st])
(defmethod build ::string  [_ v st] [(u/render-string v \') st])

;;; a keyword is an identifier if its name is not '*'
;;; if it has a namespace, it is split at '.' and each piece becomes a prefix

(s/def ::ident (s/and keyword? #(not= "*" (name %))))
(defmethod build ::ident   [_ v st] [(u/render-ident v) st])

;;; a nonbinding is a symbol whos name does not begin with '?'
;;; assuming it gets as far as printing, its name is printed as a string

(s/def ::nonbinding (s/and symbol? #(not (u/binding-symbol? %))))
(defmethod build ::nonbinding [_ v st] [(name v) st])

;;; an identy allows a keyword identifier or a nonbinding

(s/def ::identy (some-spec ::ident ::nonbinding))
(defmethod build ::identy  [_ v st] (build-some v st))

;;; an ident-atom is a keyword with no namespace

(s/def ::ident-atom  (s/with-gen (s/and ::ident u/no-ns?) g/keyword))
(derive ::ident-atom ::ident) ;; free implementation!

;;; a wild keyword's name is '*' and it signifies a wildcard
;;; prefix expansion is done as per ::ident

(s/def ::wild-kw (s/with-gen (s/and keyword? #(= "*" (name %)))
                   (fn [] (g/fmap #(keyword (name %) "*") (g/symbol)))))
(defmethod build ::wild-kw [_ v st] [(u/render-wildcard v) st])

;;; a literal is an escape hatch to insert literal sql
;;; hopefully you won't need it

(s/def ::literal (s/cat :isa (named literal) :literal string?))
(defmethod build ::literal [_ {:keys [literal]} st] [literal st])

;;; free is how you do operator application, it's pretty free-form

(s/def ::free (s/cat :isa (named free) :exprs (s/+ ::expr)))
(defmethod build ::free    [_ {:keys [exprs]} st] (paren-space-some exprs st))

;;; function_name ( expr* )

(s/def ::funcall    (s/cat :isa (named funcall) :fun ::identy :args (s/* ::expr)))
(defmethod build ::funcall [_ {:keys [fun args]} st] 
  (let [[fun  st1] (build ::identy fun st)
        [args st2] (paren-comma-some args st1)]
    [(str fun args) st2]))

;;; expr AS name

(s/def ::alias      (s/cat :isa (named as) :name ::ident-atom :expr ::expr))
(defmethod build ::alias [_ {:keys [name expr]} st]
  (let [[n st1] (build ::ident-atom name st)
        [e st2] (build ::expr expr st1)]
    [(str n " as " e) st2]))

;;; expressions are many things. expr+ is an expression or an alias

(s/def ::expr (some-spec ::int ::bool ::float ::string ::binding ::ident
                         ::magic ::wild-kw ::funcall ::free ::literal))
(s/def ::expr+ (some-spec ::alias ::int ::bool ::float ::string ::binding ::ident
                          ::magic ::wild-kw ::funcall ::free ::literal))
(defmethod build ::expr  [_ v st] (build-some v st))
(defmethod build ::expr+ [_ v st] (build-some v st))

;;; LIMIT count

(s/def ::limit  (s/and integer? pos?))
(defmethod build ::limit [_ v st] [(u/prefix "limit"  (str v)) st])

;;; OFFSET count

(s/def ::offset (s/and integer? pos?))
(defmethod build ::offset [_ v st] [(u/prefix "offset" (str v)) st])

;;; WHERE expr

(s/def ::where ::expr)
(defmethod build ::where  [_ v st] (u/prefix "where"  (build-some v st)))

;;; HAVING expr+

(s/def ::having ::expr)
(defmethod build ::having [_ v st] (u/prefix "having" (comma-some v st)))

;;; SELECT fields

(s/def ::select (s/coll-of ::expr+ :into [] :min-count 1))
(defmethod build ::select [_ v st] (comma-some v st))

;;; DISTINCT / DISTINCT-ON

(s/def ::distinct-on (s/cat :isa (named on) :exprs (s/+ ::expr)))
(s/def ::distinct    (s/or :distinct (named distinct) :distinct-on ::distinct-on))
(defmethod build ::distinct [_ v st]
  (match v
    [:distinct-on {:exprs exprs}] (u/prefix "select distinct on" (paren-comma-some exprs st))
    :else ["select distinct" st]))

;;; GROUP BY grouping_elem

(s/def ::group-by.rollup (s/cat :op (named rollup)        :exprs (s/+ ::expr)))
(s/def ::group-by.cube   (s/cat :op (named cube)          :exprs (s/+ ::expr)))
(s/def ::group-by.sets   (s/cat :op (named grouping-sets) :elems  (s/+ ::group-by.elem)))
(s/def ::group-by.elem   (some-spec ::group-by.rollup ::group-by.cube ::group-by.sets ::expr))
(s/def ::group-by        (s/coll-of ::group-by.elem))
(defmethod build ::group-by.rollup [_ {:keys [exprs]} st] (u/prefix "rollup"        (paren-comma-some exprs st)))
(defmethod build ::group-by.cube   [_ {:keys [exprs]} st] (u/prefix "cube"          (paren-comma-some exprs st)))
(defmethod build ::group-by.sets   [_ {:keys [elems]} st] (u/prefix "grouping sets" (paren-comma-some elems st)))
(defmethod build ::group-by        [_ v st] (u/prefix "group by" (comma-some v st)))

;;; ORDER BY order_item

(s/def ::order (s/or :simple (named asc desc) :complex (s/cat :using (named using) :op symbol?)))
(defmethod build ::order [_ v st]
  (match v
    [:simple s]         [(name v) st]
    [:complex {:op op}] [(str "using " (name op)) st]))

(s/def ::order-by.item (s/cat :expr ::expr :order ::order :nulls (s/? (named nulls-first nulls-last))))
(defmethod build ::order-by.item [_ {:keys [expr order nulls]} st]
  (let [[e st1] (build-some expr st)
        [o st2] (build ::order order st1)
        n (if nulls (u/undashed-name nulls) "")]
    [(str/join " " [e o n]) st2]))

(s/def ::order-by (s/coll-of ::order-by.item))
(defmethod build ::order-by [_ v st] (u/prefix "order-by" (comma-some v st)))

;;; FOR lock_strength ( OF table ) ( NOWAIT | SKIP LOCKED )

(s/def ::for.strength (named update no-key-update share key-share))
(s/def ::for.of (s/cat :of (named of) :idents (s/+ ::ident)))
(s/def ::for (s/cat :strength ::for.strength :of (s/? ::for.of) :wait (s/? (named nowait skip-locked))))

(defmethod build ::for [_ {:keys [strength of wait]} st]
  (letfn [(prn-of [of]
            (if (seq (:idents of))
              (str "of " (str/join ", " (map u/render-ident (:idents of))))
              ""))]
    (let [strength (u/undashed-name (str strength))
          of (prn-of of)
          wait (if wait (u/undashed-name (str wait)) "")]
      [(str/join " " ["for" strength of wait]) st])))

;;; ( UNION | INTERSECT | EXCEPT ) ( ALL | DISTINCT ) select_query

(s/def ::set-op (s/cat :op (named union intersect except)
                       :distinctness (named all distinct) :query ::select-query))
(defmethod build ::set-op [_ {:keys [op distinctness query]} st]
  (let [distinct (if distinctness (str (name distinctness) " ") "")]
    (u/prefix (str (name op) " " distinct)
            (build ::select-query query))))

;;; WITH ( RECURSIVE ) ? query_name ( id+ ) ? with_query

(s/def ::with.item (s/cat :recursive? (s/? (named recursive))
                          :name ::ident-atom
                          :args (s/? (s/coll-of ::ident-atom :type vector?))
                          :query ::with-query))
(defmethod build ::with.item [_ {:keys [recursive? name args verb query]} st]
  (let [rec (if recursive? "recursive " "")
        [args st1] (if (seq args) (paren-comma-build ::ident args st) ["" st])
        [q    st2] (build ::with-query [verb query] st1)]
    [(str rec args " as (" q ")") st2]))

(s/def ::with (s/coll-of ::with.item))
(defmethod build ::with [_ v st] (u/prefix "with" (build-some v st)))

(s/def ::column-aliases (s/coll-of ::ident-atom :kind vector?))
(s/def ::alias+cols? (s/cat :op (named as) :name ::ident-atom :col-aliases (s/? ::column-aliases)))

;;; TABLESAMPLE sampling_method ( float+ ) ( REPEATABLE seed ) ?

(s/def ::sample.args    (s/coll-of ::float :kind vector? :min-count 1))
(s/def ::sample.seed    (s/cat :op (named seed) :val number?))
(s/def ::sample         (s/cat :op (named sample) :method symbol?
                               :args ::sample.args :seed (s/? ::sample.seed)))
(defmethod build ::sample [_ {:keys [method args seed]} st]
  (let [args (str/join "," args)
        seed (if seed (str " repeatable " seed) "")]
    [(str method "(" args ")" seed) st]))

;;; FROM table_name ( ONLY ) ? ( AS alias ( column_alias+ ) ? ) ? ( tablesample_clause ) ?

(s/def ::from.table     (s/cat :op (named table) :table ::ident :only? (s/? (named only))
                               :as (s/? ::alias+cols?)
                               :sample (s/? ::sample)))
(defmethod build ::from.table [_ {:keys [table only? as sample]} st]
  (let [only (if only? " only " " ")
        [as st1] (if as (build ::as as st) ["" st])
        [sample st2] (if sample (build ::sample sample st1) ["" st1])]
    [(str table only as sample) st2]))

;;; FROM with_query_name ( AS alias ( column_alias+ ) ? ) ?

(s/def ::from.with      (s/cat :op (named with) :query ::ident-atom :as (s/? ::alias+cols?)))

;;; FROM ( LATERAL ) ? select_query ( AS alias ( column_alias+ ) ? ) ?

(s/def ::from.select    (s/cat :op (named select) :lateral (s/? (named lateral)) :query ::select-query
                               :as (s/? ::alias+cols?)))

;;; FROM ( LATERAL ) ? function_name ( arg* ) ( WITH ORDINALITY ) ? ( AS alias ( column_alias+) ? ) ?

(s/def ::from.funcall   (s/cat :op (named funcall) :lateral (s/? (named lateral))
                               :with-ordinality (s/? (named with-ordinality))
                               :fun ::identy :args (s/coll-of ::expr :kind vector?)
                               :as (s/? ::alias+cols?)))

;;; FROM from_item join_type JOIN from_item 

(s/def ::from.join.on (s/cat :op (named on) :expr ::expr))
(s/def ::from.join.using (s/cat :op (named using) :cols (s/+ ::ident-atom)))
(s/def ::from.join (s/cat :op (named join) :direction (named left right cross full)
                          :left ::from :right ::from
                          :cond (s/? (s/or :on ::from.join.on :using ::from.join.using))))
(s/def ::from (s/or :ident ::ident :table ::from.table :with ::from.with
                    :select ::from.select :funcall ::from.funcall :join ::from.join))

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

;;; WINDOW ( window_name AS ( window_definition ) ) +
;;; we don't track enough information to enforce the validity of window clauses
;;; for example, if you define a window with a partition clause, then base a new
;;; one on that existing one and give it a partition clause, it will error
;;; we do however check start and end frame clauses properly

(s/def ::window.existing     (s/cat :op (named based-on) :id ::ident))
(s/def ::window.partition    (s/cat :op (named partition-by) :exprs (s/+ ::expr)))

;;; ROWS ( CURRENT ROW | ( UNBOUNDED | count ) ( PRECEDING | FOLLOWING ) )

(s/def ::window.frame.simple (named current-row unbounded-preceding unbounded-following))
(defmethod build ::window.frame.simple [_ v st] [(u/undashed-name v) st])

(s/def ::window.frame.vector (s/cat :count (s/and integer? pos?) :marker (named preceding following)))
(defmethod build ::window.frame.vector [_ {:keys [count marker]} st] [(str count " " marker) st])

(s/def ::window.frame.any    (some-spec ::window.frame.simple ::window.frame.vector))

;;; ROWS ( frame_start | BETWEEN frame_start AND frame_end )

(s/def ::window.frame.start  (s/and ::window.frame.any (u/name-not= "unbounded-following")))
(s/def ::window.frame.end    (s/and ::window.frame.any (u/name-not= "unbounded-preceding")))
(s/def ::window.frame        (s/cat :op (named frame) :start ::window.frame.start :end (s/? ::window.frame.end)))

(defmethod build ::window.frame [_ {:keys [start end]} st]
  (let [[s st1] (build-some start st)]
    (if (seq end)
      (let [[e st2] (build-some end st1)]
        [(str "rows between " s " and " e) st2])
      (u/prefix "rows " (build-some start st)))))

;;; WINDOW ( window_name AS ( window_definition ) ) +

(s/def ::window.item (s/cat :existing     (s/? ::window.existing)
                            :partition-by (s/? ::window.partition)
                            :order-by     (s/? ::order-by)
                            :frame        (s/? ::window.frame)))
(defmethod build ::window.item [_ {:keys [existing partition-by order-by frame]} st]
  (let [e (if existing (u/render-ident (:id existing)) "")
        [p st1] (if (not partition-by) ["" st]
                    (u/prefix "partition by" (update (comma-some (:exprs partition-by) st))))
        [o st2] (if order-by (build ::order-by order-by st1) ["" st1])
        [f st3] (build ::window.frame frame st2)]
    [(str/join " " (filter #(not= "" %) [e p o f]))
     st3]))

(s/def ::window (s/coll-of ::window.item :into []))
(defmethod build ::window [_ v st] (u/prefix "window" (comma-some v st)))

;;; Queries

(s/def ::verb       #{:select :insert :update :delete :table :values})

;;; SELECT

(s/def ::select-query (s/keys :req-un [::verb ::select]
                              :opt-un [::with ::distinct ::from ::where ::window
                                       ::set-op ::group-by ::order-by ::limit ::for]))

(defmethod build ::select-query
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

;;; INSERT

(s/def ::insert-query (s/keys :req-un [::verb]))

;;; UPDATE

(s/def ::update-query (s/keys :req-un [::verb]))

;;; DELETE

(s/def ::delete-query (s/keys :req-un [::verb]))

;;; TABLE table

(s/def ::table ::ident)
(s/def ::table-query  (s/keys :req-un [::verb ::table]))
(defmethod build ::table-query [_ {:keys [table]} st]
  [(str "table " (u/render-ident table)) st])

;;; VALUES ( expr+ )

(s/def ::values (s/coll-of ::expr :into []))
(s/def ::values-query (s/keys :req-un [::verb ::values]))
(defmethod build ::values-query [_ {:keys [values]} st]
  (u/prefix "values" (paren-comma-some values st)))

(s/def ::with-query (some-spec ::select-query ::insert-query ::update-query
                               ::delete-query ::table-query  ::values-query))
(defmethod build ::with-query [_ v st] (build-some v st))

(defn generate [spec val st]
  (build spec (ss/conform! spec val) st))

