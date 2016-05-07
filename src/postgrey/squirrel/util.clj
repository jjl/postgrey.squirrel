(ns postgrey.squirrel.util
  (:require [clojure.string :as str]))

(defn kw-chop
  "Chops the `<>` off a keyword ending in `<>`, preserving namespace
   args: [keyword]
   returns: keyword"
  [k]
    (let [^String n (name k)
          n (.substring n 0 (- (.length n) 2))
          ns (namespace k)]
      (if ns
        (keyword (str ns "/" n))
        (keyword n))))

(defn kw->ident
  "Generates a sql identifier from a keyword. Respects namespace if present
   args: [name]
   returns: sql string"
  [n]
  (letfn [(piece [n] (str \" (str/replace n "\"" "\"\"") \"))]
    (if-let [ns (.getNamespace n)]
      (str (piece ns) \. (piece (.getName n)))
      (piece (.getName n)))))

(defn boolean?
  "True if the passed value is a boolean
   args: [val]
   returns: boolean"
  [n]
  (contains? #{true false} n))
