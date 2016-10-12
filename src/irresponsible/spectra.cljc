(ns irresponsible.spectra
  (:require [clojure.spec :as s]
            [clojure.spec.gen :as g]))

(defmacro some-spec
  "Given a list of specs, constructs an or using the specs as both names and values
   such that when a spec succeeds, the name is the spec that succeeded
   ex: (some-spec ::my-spec ::your-spec)
        ;; => (spec/or ::my-spec ::my-spec ::your-spec ::your-spec)
   args: [s & specs]
   returns: an or form"
  [& specs]
  (when-not (seq specs)
    (throw (ex-info "union: expected at least one spec" {:got specs})))
  `(s/or ~@(interleave specs specs)))

(s/fdef some-spec
  :args (s/+ keyword?)
  :ret (s/cat :or `#{s/or} :specs (s/+ keyword?)))

(defn first-matching-spec
  [ss v]


(defmacro overkill
  "Like some-spec, but uses multimethods
   args: [s & specs]"
  [& specs]
  (when-not (seq specs)
    (throw (ex-info "union: expected at least one spec" {:got specs})))
  (let [meth `overkill#]
    `(do (defmulti ~meth any?)
         (defmethod ~meth true [v#]
           (first-matching-spec ~specs v#)))))
    
(defn conform!
  "Conforms the value and throws an exception if ::clojure.spec/invalid is returned
   ex: (conform! integer? 123)  ;; => 123
       (conform! integer? 1.23) ;; throws!
   args: [spec val]
   returns: conformed value
   throws: if val does not conform to spec"
  [spec val]
  (let [r (s/conform spec val)]
    (if (= ::s/invalid r)
      (->> {:got  val   :spec/form     (s/form spec)
            :spec spec  :spec/describe (s/describe spec)}
           (ex-info (str "Failed spec: " (s/explain-str spec)))
           throw)
      r)))
