(ns postgrey.squirrel
  (:require [clojure.core.match :refer [match]]
            ;; [clojure.spec :as s]
            ;; [com.rpl.specter :refer []]
            ;; [com.rpl.specter.macros #?(:clj :refer :cljs :refer-macros) []]
            [postgrey.util :as u :refer [match-> match->> fatal]])
  (:import [java.lang StringBuilder]))


(declare compile-crud compile-query)

;; (defn expr [thing state])
;; "     expr
;;      {:name expr}
;;      :foo - column foo
;;      :foo/bar - column bar in table foo
;;      :foo/bar.baz column bar.baz in table foo
;;      \"foo.bar\" - column foo.bar
;;      :foo.bar/baz - column baz in table bar in schema foo
;;      :foo/* - all fields in table foo
;;      {:bar :foo} - as :foo, but alias as :baz
;;      {:baz :foo/bar} - as :foo/bar, but alias as baz
;;      {:baz :foo.bar/baz} - as :foo.bar/baz, but alias as baz
;;      [:bar :foo] - column foo in table bar
;;      [:bar :*] - all columns in table bar
;;      [:bar "*"] - column * in table bar
;;      ["*" "*" :bar] - column bar in table * in schema * - you get the idea
;;    These are invalid:
;;      [:bar :* :baz]   - Wildcards must be the last item, maybe you wanted \"*\" ?
;;      [:bar/baz :quux] - You can't use namespaced keywords in vector parts, split them out
;;      {:bar :foo/*}    - You can't give one alias to many fields
;;      {:bar [:foo :*]} - You can't give one alias to many fields
;;      (lit \"foo\") -
;;  "

;; (defn fields-map
;;   "args: [fields state]
;;      fields is a map where keys alias expressions
;;        - keys should be non-namespaced keywords
;;        - values should be expressions (see `expr`)
;;    returns: state"
;;   [m st]
;;   (let [b (:builder st)]
;;     (-> (fn [st k v]
;;           (when-not (u/nsless-kw? k)
;;             (fatal "Expected namespaceless keyword for alias" {:got k}))
;;           (let [st2 (expr v st)]
;;             (-append b " AS " (name k))
;;             st2))
;;         (reduce-kv st2 (into (sorted-map) m)))))

;; (foo [args] {opts}) ;; complex logic
;; true, 123, 123.45 ;; simple things are just printed
;; :foo :foo/bar ;; keywords are references
;; 'current_timestamp ;; symbols are keywords
;; 
;; "
;; 
;; 
;; 
;; 

;; given table foo (id serial primary key)
;; shared keys:
;;   :lateral  bool, default false
;;   :with-ordinality  bool, defualt false
;; tablesample_clause:
;;   workings:
;;     tablesample method (argument...)
;;     tablesample method (argument...) repeatable (seed)
;;   keys:
;;     :tablesample [method & args]
;;     :tablesample/seed seed
;; variation 1:
;;   workings:
;;     from foo
;;       opt: tablesample_clause, only, * after table:
;;       opt: as bar
;;         opt: ( column_alias... )
;;   keys:
;;     :only true ; optional default false
;;     :table name ; xor(this, next) required
;;     :table/as name ; optional
;;     :cols/as [ name... ] ; optional, not permitted unless :alias present
;; variation 2:
;;   workings:
;;     from ( select_query ) [as foo]
;;       opt: lateral
;;       opt: alias
;;         opt: ( column_alias... )
;;   keys:
;;     :query select-query ; required
;;     :query/as ; opt
;;     :cols/as  ; opt, only valid if :query/as
;; variation 3:
;;   workings:
;;     from with_query_name) [as foo]
;;       opt: alias
;;         opt: ( column_alias... )
;;   keys:
;;     :with/query with_query_name ; required
;;     :with/as name ; optional
;;     :cols/as names ; optional, not permitted unless :with/as present
;; variation 4: function_name ( [ argument [, ...] ] )
;;              [ WITH ORDINALITY ] [ [ AS ] alias [ ( column_alias [, ...] ) ] ]
;;   workings:
;;     from func(args...)
;;       opt: with ordinality
;;       opt: as alias
;;         opt: column_alias...
;;   keys: :with-ordinarily
;;     :func [name & args]
;;     :func/as name ; optional
;;     :cols/as ; optional, only valid with :func/as
;; variation 5: function_name ( [ argument [, ...] ] ) AS [ alias ] ( column_definition [, ...] )
;;   workings:
;;     from function_name (args...) as alias ( column_def... )
;;       opt: alias
;;         opt: column_definition...
;;   keys:
;;     :func [name & args]
;;     :func/as name ; opt
;;     :cols/defs ; required
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

;; join
;;   :left from_item
;;   :right from_item
;;   :on condition | column_list

;; and grouping_element can be one of:
;; ( )
;; expression
;; ( expression [, ...] )
;; ROLLUP ( { expression | ( expression [, ...] ) } [, ...] )
;; CUBE ( { expression | ( expression [, ...] ) } [, ...] )
;; GROUPING SETS ( grouping_element [, ...] )
;; TABLE [ ONLY ] table_name [ * ]
;; (defn grouping-elem [e state])

;; ;; [ GROUP BY grouping_element [, ...] ]
;; (defn group-by [g state]
;;   (when-not (vector? g)
;;     (fatal "group-by must be a vector"))
;;   (comma-separated grouping-elem g state))

;; ;; [ HAVING condition [, ...] ]
;; (defn having [h state]
;;   (expr h (append st " having ")))

;; (defn window-def [w state]
;;   (fatal "windows are not supported" {}))
;; ;; [ WINDOW window_name AS ( window_definition ) [, ...] ]
;; (defn window [w state]
;;   (when-not (map? w)
;;     (fatal "group-by must be a vector"))
    
;; ;; [ { UNION | INTERSECT | EXCEPT } [ ALL | DISTINCT ] select ]
;; (defn union [u state]
;;   (fatal "union is not supported" {}))
;; (defn intersect [i state]
;;   (fatal "intersect is not supported" {}))
;; (defn except [i state]
;;   (fatal "except is not supported" {}))
;; (defn unionish [thing state]
;;   state
;;   ) ;; only allow one, handle :all, :distinct
  
;; ;; [ ORDER BY expression [ ASC | DESC | USING operator ] [ NULLS { FIRST | LAST } ] [, ... ] ]
;; (defn order-by [q state])

;; ;; [ LIMIT { count | ALL } ]
;; (defn limit [count state]
;;   (expr count (append state " limit ")))

;; ;; [ OFFSET start [ ROW | ROWS ] ]
;; (defn offset [count state]
;;   (expr count (append state " offset ")))

;; ;; [ FOR { UPDATE | NO KEY UPDATE | SHARE | KEY SHARE } [ OF table_name [, ...] ] [ NOWAIT | SKIP LOCKED ] ]
;; (defn select-locking-clause [q state])

;; (defn compile-select [q state]
;;   (match->> state
;;     {:with with-qs} (comma-separated with-query with-qs)
;;     {:select _}     (select-expr q)
;;     _               (fields-list (:fields q))
;;     {:from froms}   (from-clause froms)
;;     {:where w}      (where w)
;;     {:group-by gb}  (group-by gb)
;;     {:having h}     (having h)
;;     {:window w}     (window w)
;;     _               (unionish q)
;;     {:order-by o}   (order-by o)
;;     {:limit l}      (limit )
;;     {:offset o}     (offset o)
;;     _               (select-locking-clause q)))

;; (def query-compilers
;;   {:select compile-select
   ;; :insert compile-insert
   ;; :update compile-update
   ;; :delete compile-delete
   ;; :abort compile-abort
   ;; :alter/aggregate
   ;; :alter/collation
   ;; :alter/converstoin
   ;; :alter/database
   ;; :alter/default-privileges
   ;; :alter/domain
   ;; :alter/event-trigger
   ;; :alter/extension
   ;; :alter/foreign-data-wrapper
   ;; :alter/foreign-table
   ;; :alter/function
   ;; :alter/group
   ;; :alter/index
   ;; :alter/language
   ;; :alter/large-object
   ;; :alter/materialized-view
   ;; :alter/operator
   ;; :alter/operator-class
   ;; :alter/operator-family
   ;; :alter/policy
   ;; :alter/role
   ;; :alter/rule
   ;; :alter/schema
   ;; :alter/sequence
   ;; :alter/server
   ;; :alter/system
   ;; :alter/table
   ;; :alter/tablespace
   ;; :alter/text-search-configuration
   ;; :alter/text-search-dictionary
   ;; :alter/text-search-parser
   ;; :alter/text-search-template
   ;; :alter/trigger
   ;; :alter/type
   ;; :alter/user
   ;; :alter/user-mapp
   ;; :alter/view
   ;; :analyze
   ;; :begin
   ;; :checkpoint
   ;; :close
   ;; :cluster
   ;; :comment
   ;; :commit
   ;; :commit/prepared
   ;; :copy
   ;; :create/aggregate
   ;; :create/cast
   ;; :create/collation
   ;; :create/conversion
   ;; :create/database
   ;; :create/domain
   ;; :create/event-trigger
   ;; :create/extension
   ;; :create/foreign-data-wrapper
   ;; :create/foreign-table
   ;; :create/function
   ;; :create/group
   ;; :create/index
   ;; :create/language
   ;; :create/materialized-view
   ;; :create/operator
   ;; :create/operator-class
   ;; :create/operator-family
   ;; :create/policy
   ;; :create/role
   ;; :create/rule
   ;; :create/sequence
   ;; :create/server
   ;; :create/table
   ;; :create/table-as
   ;; :create/tablespace
   ;; :create/text-search-configuration
   ;; :create/text-search-dictionary
   ;; :create/text-search-parser
   ;; :create/text-search-template
   ;; :create/transform
   ;; :create/trigger
   ;; :create/type
   ;; :create/user
   ;; :create/user-mapping
   ;; :create/view
   ;; :deallocate
   ;; :declare
   ;; :delete
   ;; :discard
   ;; :do
   ;; :drop/aggregate
   ;; :drop/cast
   ;; :drop/collation
   ;; :drop/conversion
   ;; :drop/database
   ;; :drop/domain
   ;; :drop/event-trigger
   ;; :drop/extension
   ;; :drop/foreign-data-wrapper
   ;; :drop/foreign-table
   ;; :drop/function
   ;; :drop/group
   ;; :drop/index
   ;; :drop/language
   ;; :drop/materialized-view
   ;; :drop/operator
   ;; :drop/operator-class
   ;; :drop/operator-family
   ;; :drop/owned
   ;; :drop/policy
   ;; :drop/role
   ;; :drop/rule
   ;; :drop/schema
   ;; :drop/sequence
   ;; :drop/server
   ;; :drop/table
   ;; :drop/tablespace
   ;; :drop/text-search-configuration
   ;; :drop/text-search-dictionary
   ;; :drop/text-search-parser
   ;; :drop/text-search-template
   ;; :drop/transform
   ;; :drop/trigger
   ;; :drop/type
   ;; :drop/user
   ;; :drop/user-mapping
   ;; :drop/view
   ;; :end
   ;; :execute
   ;; :explain
   ;; :fetch
   ;; :grant
   ;; :import/foreign-schema
   ;; :listen
   ;; :load
   ;; :lock
   ;; :move
   ;; :notify
   ;; :prepare
   ;; :prepare/transaction
   ;; :reassign/owned
   ;; :refresh/materialized-view
   ;; :reindex
   ;; :release/savepoint
   ;; :reset
   ;; :revoke
   ;; :rollback
   ;; :rollback/prepared
   ;; :rollback/to-savepoint
   ;; :savepoint
   ;; :security/label
   ;; :select-into
   ;; :set
   ;; :set/constraints
   ;; :set/role
   ;; :set/session-authorization
   ;; :set/transaction
   ;; :show
   ;; :start/transaction
   ;; :truncate
   ;; :unlisten
   ;; :vacuum
   ;; :values
;;    })

;; (defn compile-crud [q state]
;;   (match q
;;     {:select _} (compile-select q state)
;;     {:insert _} (fatal "sorry, we only support select queries at the minute" {:got q}) ;;(compile-insert q state)
;;     {:update _} (fatal "sorry, we only support select queries at the minute" {:got q}) ;;(compile-update q state)
;;     {:delete _} (fatal "sorry, we only support select queries at the minute" {:got q}) ;;(compile-delete q state)
;;     :else       (fatal "Expected a select, insert, update or delete query" {:got q})))

;; (defn compile-query [q state]
;;   (match q
;;     {:select _} (compile-select q state)
;;     {:insert _} (compile-insert q state)
;;     {:update _} (compile-update q state)
;;     {:delete _} (compile-delete q state)
;;     :else (fatal "Expected a valid query" {:got q})))

;; (defn ?
;;   "Returns a placeholder with the given name (keyword)
;;    args: [name]
;;    returns: Placeholder"
;;   [name]
;;   (i/->Placeholder name))
