(ns postgrey.squirrel
  (:require [postgrey.squirrel.internal :as i]))

(defn ?
  "Returns a placeholder with the given name (keyword)
   args: [name]
   returns: Placeholder"
  [name]
  (i/->Placeholder name))

