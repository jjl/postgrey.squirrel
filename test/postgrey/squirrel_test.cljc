(ns postgrey.squirrel-test
  (:require [postgrey.squirrel :as s]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

;; (deftest ?
;;   (let [p (s/? :foo)]
;;     (is (instance? i/Placeholder p))
;;     (is (= :foo (:name p)))))
        
