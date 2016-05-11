(ns postgrey.squirrel.util
  (:require [clojure.string :as str]
            [clojure.core.match :refer [match]]))

(defn fnish-kw?
  "True if passed a keyword ending in <>
   args: [val]
   returns: bool"
  [s]
  (and (keyword? s)
       (-> s ^String name (.endsWith "<>"))))

(defn kw-chop
  "Chops the `<>` off a keyword ending in `<>`, preserving namespace
   args: [keyword]
   returns: keyword"
  [k]
  (if (fnish-kw? k)
    (let [^String n (name k)
          n (.substring n 0 (- (.length n) 2))
          ns (namespace k)]
      (if ns
        (keyword (str ns "/" n))
        (keyword n)))
    k))

(defn kw->ident
  "Generates a sql identifier from a keyword. Respects namespace if present
   args: [name]
   returns: sql string"
  [n]
  (letfn [(piece [n] (str \" (str/replace n "\"" "\"\"") \"))]
    (if-let [ns (.getNamespace n)]
      (str (piece ns) \. (piece (.getName n)))
      (piece (.getName n)))))

(defn kw->op
  [k]
  (when (namespace k)
    (throw (ex-info "Sorry, we haven't figured out namespaced operators yet" {})))
  (name k))    

(def funkw->ident
  "Given a function-keyword (ending in <>), turns it into an identifier string
   args: [name]
   returns: [sql string]"
  (comp kw->ident kw-chop))

(defn boolean?
  "True if the passed value is a boolean
   args: [val]
   returns: boolean"
  [n]
  (contains? #{true false} n))

(def simple?
  "True if the value passed is an integer of some kind or a boolean
   args: [val]
   returns: boolean"
  (some-fn boolean? integer?))

(def fny? (some-fn ifn? symbol?))

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
          binds (map (fn [r] [sym r]) ns)
          forms (map #(state-form sym %) clauses)
          lets (interleave binds forms)]
      `(let [~sym ~init ~@lets]
         [~sym [~@ns]]))
    init))

(defn smap
  ""
  [f state coll]
  (loop [st state
         rs (transient [])
         cs coll]
    (if (seq cs)
      (let [[c & cs_] cs
            [st2 r] (f st c)]
        (recur st2 (conj! rs r) cs_))
      [st (persistent! rs)])))

(defn skeep
  ""
  [f state coll]
  (into [] (keep identity (smap f state coll))))

(defn fatal [msg data]
  (throw (ex-info (str msg " " (pr-str data)) data)))
