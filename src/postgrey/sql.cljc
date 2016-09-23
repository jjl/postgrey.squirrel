(ns postgrey.sql
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.spec :as s]
            [clojure.spec.gen :as gen]
            [guten-tag.core :as t]
            [taoensso.truss :as truss #?(:clj :refer :cljs :refer-macros) [have have! have?]]
            [com.rpl.specter #?(:clj :refer :cljs :refer-macros) [select]]
            [postgrey.state :as st
             :refer [append prefix suffix maybe-prefix maybe-suffix error
                     comma-sep dot-sep parens hold-alias warning]]
            [postgrey.util :as u :refer [match-> match->> s->]]))

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

(defn leaf-descendants
  "Returns only the leaf descendants of tag from the global hierarchy"
  [tag]
  (->> tag descendants
       (into #{} (filter #(not (seq (descendants %)))))))

(t/deftag state [placeholders]
  ""
  {:pre [#(set? placeholders)
         #(every? string? placeholders)]})

(defn new-state []
  (->state #{}))

(defn try-place
  "Adds a placeholder to the state, unless it already exists in which case error
   ex: (try-place state :foo) ; this state has now seen the :foo placeholder
   args: [state ph-name]
     ph-name: a non-empty string naming the placeholder
   returns: state"
  [name {:keys [placeholders] :as state}]
  (if (contains? placeholders name)
    (-> (str "Placeholder already exists: " name)
        (ex-info {:name name :placeholders placeholders})
        throw)
    (update state :placeholders conj name)))

(defmulti build
  "Turns a thing into sql
   args: [type thing state & extra]
     type:  a qualified keyword naming a spec
     thing: the data to turn into sql
     state: state map
   returns: type
   contract:
     defmethods should return a 2-tuple: [sql state]
       sql: a snippet of sql as a string
       state: a new state object"
  (fn [a _ _ & _] a))

(defn try-build-leaf
  "Given a tag and a value, looks up the leaf descendants of tag
   and validates v against them until one matches, then builds it
   args: [tag val state]
   returns: [ret state]"
  [t v st]
  (loop [cs (leaf-descendants t)]
    (when-let [[c & cs2] (seq cs)]
      (prn :got c :valid? (s/valid? c v))
      (if (s/valid? c v)
        (build c v st)
        (recur cs2)))))

(defn collect [type v st & args]
  (reduce (fn [[acc st] t]
            (let [[r st2] (apply build type t st args)]
              [(conj acc r) st2]))
          [[] st] v))

(defn collect-join [type v st sep & args]
  (let [[rs st1] (apply collect type v st args)]
    [(str/join sep rs) st1]))

(s/def ::integer integer?)

(defmethod build ::integer
  [_ v st]
  [(str v) st])

(s/def ::float float?)

(defmethod build ::float
  [_ v st]
  [(str v) st])

(s/def ::string string?)

(defn render-string [str delim]
  (str delim (str/replace str delim (str delim delim) delim)))

(defmethod build ::string
  [_ v st]
  [(render-string v \') st])

(s/def ::pos-string (s/and string? #(not= "" %)))
(derive ::pos-string ::string)

(s/def ::ident-atom-string (s/and string? #(not= "" %)))

(defmethod build ::ident-atom-string
  [_ v st]
  [(render-string v \") st])

(s/def ::ident-atom-kw (s/and keyword? #(not (namespace %)) #(not= :* %)))

(defmethod build ::ident-atom-kw
  [_ v st]
  [(render-string (name v) \") st])

(s/def ::ident-kw (s/and keyword? #(not= "*" (name %))))

(defmethod build ::ident-kw
  [_ v st]
  (let [ns (namespace v)
        n  (name v)]
    (if ns
      (collect-join ::ident-atom-string [ns n] st ".")
      (build ::ident-atom-string n st))))

(s/def ::basic (s/or :int ::integer :float ::float :string ::string :kw ::ident-kw))
(derive ::integer ::basic)
(derive ::float   ::basic)
(derive ::string  ::basic)

(s/def ::pos-int (s/and ::integer #(> % 0)))

(defmethod build ::basic
  [t v st]
  (or (try-build-leaf t v st)
      (throw (ex-info "Expected int, float or string" {:got v :valid (descendants t)}))))

(t/deftag placeholder [name]
  ""
  {:pre [#(s/valid? ::pos-string name)]})

(s/def ::placeholder placeholder?)

(defmethod build ::placeholder
  [_ {:keys [name]} st]
  ["?" (try-place name st)])

(t/deftag literal [snippet]
  ""
  {:pre [#(s/valid? ::pos-string name)]})

(s/def ::literal literal?)

(defmethod build ::literal
  [_ {:keys [snippet]} st]
  [snippet st])

(s/def ::ident-piece (s/or :literal ::literal :kw ::ident-atom-kw :string ::ident-atom-string))
(derive ::literal           ::ident-piece)
(derive ::ident-atom-kw     ::ident-piece)
(derive ::ident-atom-string ::ident-piece)        

(defmethod build ::ident-piece
  [t v st]
  (or (try-build-leaf t v st)
      (throw (ex-info "Expected keyword, string or literal" {:got v :valid (descendants t)}))))

(s/def ::ident-atom (s/or :literal ::literal :kw ::ident-atom-kw))
(derive ::ident-atom-kw ::ident-atom)
(derive ::literal       ::ident-atom)

(defmethod build ::ident-atom
  [t v st]
  (or (try-build-leaf t v st)
      (throw (ex-info "Expected string or literal" {:got v :valid (descendants t)}))))

(t/deftag identifier [pieces]
  ""
  {:pre [#(vector? pieces)
         #(> (count pieces) 0)
         (fn [] every? #(s/valid? ::ident-piece %) pieces)]})

(s/def ::identifier identifier?)

(defmethod build ::identifier
  [_ {:keys [pieces]} st & ]
  (let [ps (collect ::ident-piece pieces st)]
    [(str/join "." ps) st]))

(s/def ::id (s/or :kw ::ident-kw :identifier ::identifier :literal ::literal))
(derive ::ident-kw   ::id)
(derive ::identifier ::id)

(defmethod build ::id
  [t v st]
  (or (try-build-leaf t v st)
      (throw (ex-info "Expected keyword, identifier or literal" {:got v :valid (descendants t)}))))

(s/def ::wildcard-kw (s/and keyword? #(= (name %) "*")))

(defmethod build ::wildcard-kw
  [_ v st]
  (if-let [ns (namespace v)]
    (update (build ::ident-string ns st) 0 str ".*")
    ["*" st]))

(t/deftag wildcard [pieces]
  ""
  {:pre [#(vector? pieces)
         (fn [] (every? #(s/valid? ::ident-atom %) pieces))]})

(defmethod build ::wildcard
  [_ v st]
  (update (collect-join ::ident-atom v st ".") 0 str ".*"))

(s/def ::wildcard (s/or :wildcard wildcard? :kw ::wildcard-kw))

(defmethod build ::identifier
  [_ {:keys [pieces]} st]
  (str/join "."
            (-> []
                (into (map #(first (build ::ident-atom % st))) pieces)
                (cons "*"))))

(t/deftag funcall [func args]
  ""
  {:pre [#(s/valid? ::id func)
         #(vector? args)
         (fn [] (every? #(s/valid? ::expr %) args))]})

(s/def ::funcall funcall?)

(defmethod build ::funcall
  [_ {:keys [func args]} st]
  (let [[v1 st1] (build ::id func st)
        [vs st2] (collect-join ::expr args st1 ", ")]
    [(str v1 "(" vs ") ") st2]))

(t/deftag prefix-op [op arg]
  ""
  {:pre [#(s/valid? ::id   op)
         #(s/valid? ::expr arg)]})

(s/def ::prefix-op prefix-op?)

(defmethod build ::prefix-op
  [_ {:keys [op arg]} st]
  (let [[o st1] (build ::id   op  st)
        [a st2] (build ::expr arg st1 true)]
    [(str o " "  a) st2]))

(t/deftag infix-op  [ops args]
  ""
  {:pre [#(vector? ops)
         #(vector? args)
         #(= (inc (count ops)) (count args))
         #(> (count ops) 0)
         (fn [] (every? #(s/valid? ::id %) ops))
         (fn [] (every? #(s/valid? ::expr) args true))]})

(s/def ::infix-op infix-op?)

(defmethod build ::infix-op
  [_ {:keys [ops args]} st]
  (let [[ops st1]  (collect ::id ops st)
        [args st2] (collect ::expr args st1 true)
        is (cons (first args) (interleave ops (rest args)))]
    [(str/join " " is) st2]))

(s/def ::op (s/or :prefix-op ::prefix-op :infix-op ::infix-op))
(derive ::prefix-op ::op)
(derive ::infix-op  ::op)

(defmethod build ::op
  [t v st]
  (or (try-build-leaf t v st)
      (throw (ex-info "Expected operator" {:got v :valid (descendants t)}))))

(s/def ::expr (s/or :placeholder ::placeholder  :basic   ::basic    :id ::id
                    :wildcard    ::wildcard     :funcall ::funcall  :op ::op))
(derive ::placeholder ::expr)
(derive ::basic       ::expr)
(derive ::id          ::expr)
(derive ::wildcard    ::expr)
(derive ::funcall     ::expr)
(derive ::op          ::expr)

(defn maybe-bracket [val bracket?]
  (if bracket?
    (str "(" val ")")
    val))

(defmethod build ::expr
  [t v st & bracket?]
  (if-let [[r st1] (try-build-leaf t v st)]
    [(maybe-bracket r bracket?) st1]
    (throw (ex-info "Expected expression" {:got v :valid (descendants t)}))))

(t/deftag alias [as exp of]
  ""
  {:pre [#(s/valid? ::id as) #(s/keyword? of) #(s/valid? of exp)]})

(s/def ::alias alias?)

(defmethod build ::alias
  [_ {:keys [as exp]} st]
  (let [[e st1] (build ::id   exp st)
        [a st2] (build ::expr as  st1)]
    [(str e " as " a) st2]))

(s/def ::expr+ (s/or :expr ::expr :alias ::alias))
(derive ::expr  ::expr+)
(derive ::alias ::expr+)

(defmethod build ::expr+
  [_ v st]
  (cond (s/valid? ::alias v) (build ::alias v st)
        (s/valid? ::expr  v) (build ::expr v st)
        :else (-> "Expected (possibly aliased) expression"
                  (ex-info {:got v})
                  throw)))

(s/def ::with.recursive? boolean?)
(s/def ::with.name ::ident-atom)
(s/def ::with.args (s/coll-of ::id))
(s/def ::with.query identity) ;; FIXME when subqueries
(s/def ::with (s/keys :req-un [::with.name ::with.query]
                      :opt-un [::with.recursive? ::with.args]))

(defmethod build ::with.args
  [_ v st]
  (if (seq v)
    (update (collect-join ::ident-atom v st ",") 0 #(str \( % \)))
    ["" st]))

;; (defmethod build ::with.query
;;   [_ v st]
;;   ...)

(defmethod build ::with
  [_ {n ::with.name q       ::with.query
      r ::with.recursive? a ::with.args
      :or {n []}} st]
  (let [r (if with.recursive? "recursive " "")
        [n st1] (build ::ident-atom with.name st)
        [a st2] (build ::with.args with.args st1)
        [q st3] (build ::with.query with.query st2)]
    [(str r n a " as " q) st3]))

(s/def ::with-clause (s/coll-of ::with))

(defmethod build ::with-clause
  [_ v st]
  (if (seq v)
    (update (collect-join ::with v st) 0 #(str "with " %))
    ["" st]))

(s/def ::distinct-on (s/cat :_ #{:distinct-on} :expr ::expr))
(s/def ::select (s/or :simple #{:all :distinct} :distinct-on ::distinct-on))

(defmethod build ::select
  [_ v st]
  (match v
         :all      ["select" st]
         :distinct ["select distinct" st]
         ([:distinct-on e]) (update (build ::expr e st true) #(str "select distinct on " %))
         :else     (throw (ex-info "expected :all, :distinct or (:distinct-on expr)" {:got v}))))

(s/def ::field (s/or :id ::id :wildcard ::wildcard :alias ::alias))
(derive ::id       ::field)
(derive ::wildcard ::field)
(derive ::alias    ::field)

(defmethod build ::field
  [t v st]
  (or (try-build-leaf t v st)
      (throw (ex-info "Expected field" {:got v :valid (descendants t)}))))

(s/def ::fields (s/coll-of ::field))

(defmethod build ::fields
  [_ v st]
  (collect-join ::field v st ", "))

;; (s/def ::from ...)

;; (defmethod build ::from
;;   [_ v st]
;;   ...)

(s/def ::where (s/coll-of ::expr))

(defmethod build ::where
  [_ v st]
  (update (s/collect-join ::expr v st " and " true) 0 #(str "where ")))

(s/def ::group-by.rollup (s/cat :rollup #{:rollup} :exprs (s/coll-of ::expr)))
(s/def ::group-by.cube (s/cat :cube #{:rollup} :exprs (s/coll-of ::expr)))
(s/def ::group-by.grouping-sets (s/cat :rollup #{:rollup} :groups (s/coll-of ::group-by)))
(s/def ::group-by.exprs         (s/cat :_ #{:exprs} :exprs (s/coll-of ::expr)))
(s/def ::group-by.elem (s/or :rollup ::group-by.rollup :cube ::group-by.cube
                             :grouping-sets ::group-by.grouping-sets :exprs ::group-by.exprs))
(s/def ::group-by (s/coll-of ::group-by.elem))
(derive ::group-by.rollup ::group-by.elem)
(derive ::group-by.cube ::group-by.elem)
(derive ::group-by.grouping-sets ::group-by.elem)
(derive ::group-by.exprs ::group-by.elem)

(defmethod build ::group-by
  [_ v st]
  (update (s/collect-join ::group-by.elem v st ", " true) 0 #(str "group by ")))

(s/def ::having (s/coll-of ::expr))

(defmethod build ::having
  [_ v st]
  (if (seq v)
    (update (collect-join ::expr v st ", ") 0 #(str "having " %))
    ["" st]))

;; (s/def ::window ...)

;; (defmethod build ::window
;;   [_ v st]
;;   ...)

;; (s/def ::unionish ...)

;; (defmethod build ::unionish
;;   [_ v st]
;;   ...)

(s/def ::order (s/or :simple #{:asc :desc} :using (s/cat :using #{:using} :id ::id)))
(s/def ::nulls #{:first :last})
(s/def ::order-by.item (s/keys :req-un [::expr ::order] :opt-un [::nulls]))
(s/def ::order-by (s/coll-of ::order-by.item))

(defmethod build ::order
  [_ v st]
  (match v
    nil   [""     st]
    :asc  ["asc"  st]
    :desc ["desc" st]
    ([:using id] :seq) (update (build ::id id st) 0 #(str "using " %))
    :else (throw (ex-info "expected one of :asc, :desc, [:using operator-id]" {:got v}))))

(defmethod build ::nulls
  [_ v st]
  (cond (= :first v) ["nulls first" st]
        (= :last  v) ["nulls last"  st]
        (nil? v)     ["" st]
        :else (throw (ex-info "expected one of :first, :last" {:got v}))))

(defmethod build ::order-by.item
  [_ {:keys [::expr ::order ::nulls]} st]
  (let [[e st2] (build ::expr  expr  st)
        [o st1] (build ::order order st)
        [n st3] (build ::nulls nulls st)]
    [(str/join " " [e o n]) st3]))

(defmethod build ::order-by
  [_ v st]
  (update (collect-join ::order-by-item v st ", ") 0 #(str "order by " %)))

(s/def ::limit ::pos-int)

(defmethod build ::limit
  [_ v st]
  (if v
    [(str "limit " v) st]
    ["" st]))

(s/def ::offset ::pos-int)

(defmethod build ::offset
  [_ v st]
  (if v
    [(str "offset " v) st]
    ["" st]))

(s/def ::lock-strength #{:update :no-key-update :share :key-share})
(s/def ::lock-wait     #{:nowait :skip-locked})

(s/def ::tables (s/coll-of ::ids))

(defmethod build ::lock-strength
  [_ v st]
  [({:update "update" :no-key-update "no-key-update"
     :share "share" :key-share "key share"} v)
   st])

(defmethod build ::lock-wait
  [_ v st]
  [({:nowait "update" :skip-locked "skip locked" nil ""} v)
   st])

(s/def ::for (s/keys :req-un [::lock-strength] :opt-un [::tables ::lock-wait]))

(defmethod build ::for
  [_ {:keys [::lock-strength ::tables ::lock-wait]} st]
  (let [[s st1]  (build ::lock-strength lock-strength)
        [ts st2] (if (seq tables)
                   (update (collect-join ::id tables st1 ", ") 0 #(str "of " %))
                   ["" st1])
        [w st3]  (build ::lock-wait  lock-wait st2)]
    [(str/join " " ["for" s ts w]) st]))

;; (defn transpile-select [q state]
;;   (match->> state
;;     {:with with-qs} (transpile-with with-qs)
;;     _               (transpile-select- q)
;;     _               (transpile-fields-list (:fields q))
;;     {:from froms}   (transpile-from-clause froms)
;;     {:where w}      (where w)
;;     {:group-by gb}  (group-by gb)
;;     {:having h}     (having h)
;;     {:window w}     (window w)
;;     _               (unionish q)
;;     {:order-by o}   (order-by o)
;;     {:limit l}      (limit )
;;     {:offset o}     (offset o)
;;     _ (select-locking-clause q)))


;; (defn select-query [q]

;; (defn nonempty-kw? [k]
;;   (and (keyword? k) (not= "" (name k))))

;; (s/def ::table  ::name)
;; (s/def ::column ::bare-name)
;; (s/def ::expr (s/or :basic ::basic :place ::place :literal ::literal)) ;; funcalls, special
;; (s/def ::condition ::expr)
;; (s/def ::aliased-exprs (s/map-of ::name ::expr))
;; (s/def ::with-query-type #{:select :insert :update :delete :values})
;; (s/def ::values (s/coll-of ::expr :into []))
;; (s/def ::with-query (s/cat :recursive? #{:recursive}
;;                            :name ::bare-name
;;                            :cols (s/? (s/coll-of ::column))
;;                            :as (s/or :values ::values
;;                                      :select ::select-query
;;                                      :insert ::insert-query :update ::update-query
;;                                      :delete ::delete-query)))
;; (s/def ::with (s/coll-of ::with-query :into [] :min-count 1 :distinct true))
;; (s/def ::select (s/or :star ::star :aliased-exprs ::aliased-exprs :expr ::expr))
;; (s/def ::distinct true?)
;; (s/def ::distinct-on (s/coll-of ::expr :into [] :min-count 1 :distinct true))
;; ;; (s/def ::from-item ...)
;; ;; (s/def ::from (s/coll-of ::from-item :min-count 1))
;; (s/def ::where ::condition)
;; (s/def ::rollup        (s/coll-of ::expr :min-count 1 :into []))
;; (s/def ::cube          (s/coll-of ::expr :min-count 1 :into []))
;; (s/def ::grouping-sets (s/coll-of ::grouping-element :min-count 1 :into []))
;; (s/def ::rollup-expr        (s/cat :grouping-op #{:rollup}
;;                                    :grouping-over ::rollup))
;; (s/def ::cube-expr          (s/cat :grouping-op #{:cube}
;;                                    :grouping-over ::cube))
;; (s/def ::grouping-sets-expr (s/cat :grouping-op #{:grouping-sets}
;;                                    :grouping-over ::grouping-sets))
;; (s/def ::grouping-element (s/or :rollup        (s/spec ::rollup-expr)
;;                                 :cube          (s/spec ::cube-expr)
;;                                 :grouping-sets (s/spec ::grouping-sets-expr)
;;                                 :expr          ::expr))
;; (s/def ::group-by (s/coll-of ::grouping-element))
;; (s/def ::having ::condition)
;; ;; (s/def ::window-def ...)
;; ;; (s/def ::window (s/map-of ::bare-name ::window-def))
;; (s/def ::order (s/or :direction #{:asc :desc} :by-operator ::name))
;; (s/def ::nulls-placement #{:nulls-first :nulls-last})
;; (s/def ::order-by-expr (s/cat :expr ::expr :order (s/? ::order) :nulls (s/? ::nulls-placement)))
;; (s/def ::order-by (s/coll-of ::order-by-expr))
;; (s/def ::limit  (every-pred integer? pos?))
;; (s/def ::offset (every-pred integer? pos?))
;; (s/def ::for-type #{:update :no-key-update :share :key-share})
;; (s/def ::for-of (s/cat :of #{:of} :table ::table))
;; (s/def ::for-locking #{:nowait :skip-locked})
;; (s/def ::for-item (s/cat :for ::for-type :of (s/? ::for-of) :locking (s/? ::for-locking)))
;; (s/def ::for (s/coll-of ::for-item :min-count 1))

;; (def query-types #{:select :insert :update :delete :values})

;; (s/def ::select-query
;;   (s/keys :req-un [::select]
;;           :opt-un [::distinct ::distinct-on ::where ::group-by ::with
;;                    ;; ::from ::window
;;                    ::having ::order-by ::limit ::offset ::for]))

;; (defmulti query-spec
;;   (comp u/one-or-all (partial set/intersection query-types) set keys))

;; (defmethod query-spec :select [_] ::select-query)
;; ;; (defmethod query-spec :insert [_] ::insert-query)
;; ;; (defmethod query-spec :update [_] ::update-query)
;; ;; (defmethod query-spec :delete [_] ::delete-query)
;; ;; (defmethod query-spec :values [_] ::values-query)
;; (defmethod query-spec #{} [_] (throw (ex-info "Invalid query!" {:valid query-types})))
;; (s/def ::query (s/multi-spec query-spec :query/type))

;; (s/conform query-spec {:select})
;; (s/exercise ::bare-name)
