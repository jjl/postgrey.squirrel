(ns postgrey.state
  (:require [clojure.core.match :refer [match]]
            [flatland.ordered.set :as os]
            [postgrey.util :as u :refer [fatal]]))

(def empty-state
  {:placeholders (os/ordered-set)
   :acc []
   :aliases (os/ordered-set)
   :errors []
   :warnings []})

(defn without-aliases [st]
  (assoc st :aliases (os/ordered-set)))

(defn replace-aliases
  "Copies the aliases from the src into the dest, returning dest
   args: [dest src]
   returns: state"
  [st1 st2]
  (assoc st1 :aliases (:aliases st2)))

(defn ok? [state]
  (empty? (:errors state)))

(defn good? [state]
  (and (ok? state)
       (empty? (:warnings state))))

(defn append
  "Appends the given strings to the state accumulator
   args: [state & strings]
   returns: state"
  [st & strings]
  (update st :acc into (flatten strings)))

(defn error
  "Appends the given error to the state error vector and appends
   a numbered placeholder into the state accumuluator
   args: [state err]
   returns: state"
  [st err]
  (-> (append st (str "<ERROR #" (count (:errors st)) ">"))
      (update :errors conj err)))

(defn warning
  "Appends the given warning to the state warning vector
   args: [state warning]"
  [st w]
  (update st :warnings conj w))

(defn put-new
  "Conjs `name` into the collection at `key` in `state`
   args: [state key name]
     key: a keyword (not checked)
       useful: :placeholders, :aliases
     name: a keyword, the value to conj (checked for kw-ness)
   returns: state
   throws: if name exists already or is not a keyword"
  [st key name]
  (if (contains? (get st key) name)
    (error st {:error "Duplicate name" :name name :source ::put-new :type key})
    (update st key conj name)))

(defn hold-place
  "Adds a placeholder with the given name to the state or an error if it exists
   args: [state name]
   returns: state"
  [st name]
  (put-new st :placeholders name))

(defn hold-alias
  "Adds an alias with the given name to the state or an error if it exists
   args: [state name]
   returns: state
   throws: if name exists already as an alias or is not a keyword"
  [st name]
  (put-new st :aliases name))

(defn prefix
  "Prepends the given string, then runs the compiler with thing
   args: [left compiler thing state]
     left: string to prepend
     compiler: a function of [thing state] -> state
     thing: argument to pass to compiler
   returns: state"
  [left compiler thing st]
  (->> (append st left)
       (compiler thing)))

(defn suffix
  "Appends the given string after running the compiler with thing
   args: [left compiler thing state]
     right: string to append
     compiler: a function of [thing state] -> state
     thing: argument to pass to compiler
   returns: state"
  [right compiler thing st]
  (-> (compiler thing st)
      (append right)))

(defn maybe-prefix
  "Like prefix, but only if test is truthy
   args: [test left compiler thing state]
     test: truthy
     left: string to prepend
     compiler: a function of [thing state] -> state
     thing: argument to pass to compiler
   returns: state"
  [test left compiler thing st]
  (if test
    (compiler thing (append st left))
    (compiler thing st)))

(defn maybe-suffix
  "Like suffix, but only if test is truthy.
   args: [test pred right compiler thing state]
     test: truthy value
     right: string to append
     compiler: a function of [thing state] -> state
     thing: argument to pass to compiler
   returns: state"
  [test right compiler thing st]
  (if test
    (append (compiler thing st) right)
    (compiler thing st)))

(defn surround
  "Wraps running the compiler with the given left and right strings
   args: [left right compiler thing state]
     left: string to prepend
     right: string to append
     compiler: a function of [thing state] -> state
     thing: argument to pass to compiler
   returns: state"
  [left right compiler thing st]
  (append (compiler thing (append st left)) right))

(defn parens
  "Wraps running the compiler with thing and surrounds with parens
   args: [compiler thing state]
     compiler: a function of [thing state] -> state
     thing: argument to pass to compiler
   returns: state"
  [compiler thing st]
  (surround "(" ")" compiler thing st))

(defn sep-by
  "Interleaves runs of (compiler thing state) with sep
   args: [compiler sep things state]
     compiler: a function of [thing state] -> state
     sep: a string to use as separator
     things: list of items to individually compile
   returns: state"
  [comp sep things st]
  (if (seq things)
    (reduce #(prefix sep comp %2 %1)
            (comp (first things) st)
            (rest things))
    st))

(defn comma-sep
  "Interleaves runs of (compiler thing state) with comma
   args: [compiler sep things state]
     compiler: a function of [thing state] -> state
     things: list of items to individually compile
   returns: state"
  [compiler things st]
  (sep-by st compiler "," things))

(defn dot-sep
  "Interleaves runs of (compiler thing state) with dots
   args: [compiler sep things state]
     compiler: a function of [thing state] -> state
     things: list of items to individually compile
   returns: state"
  [compiler things st]
  (sep-by st compiler "." things))
