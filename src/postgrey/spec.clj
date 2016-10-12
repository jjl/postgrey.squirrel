(ns postgrey.spec
  (:require [clojure.spec :as s]
            [postgrey.util :as u]))

(defmacro some-spec [ss val]
  (let [arg `a#]
    `(let [~arg ~val]
       (cond ~@(mapcat (fn [v] [`(s/valid? ~v ~arg) v]) ss)
             :else nil))))

(defn conform! [spec val]
  (let [r (s/conform spec val)]
    (if (= ::s/invalid r)
      (throw (u/fatal "Spec conform error" {:got val :spec spec :explain (s/explain spec val)}))
      r)))
