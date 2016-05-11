(ns postgrey.squirrel.state
  (:require [flatland.ordered.set :as os]))

(defrecord State [placeholders])

(def empty-state (->State (os/ordered-set)))

(defn hold-place
  "Returns a new state with a placeholder added
   args: [state ph]
     ph must be a Placeholder (obtained through `?`)
   returns: state"
  [state ph]
  (let [n (name ph)]
    (if ((:phs state) n)
      (throw (ex-info (str "Cannot add placeholder (already exists): " n) {}))
      (update state :placeholders conj n))))
  
