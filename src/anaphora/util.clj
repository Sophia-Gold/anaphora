(ns anaphora.util
  (:require [clojure.walk :refer [macroexpand-all]]
            [com.rpl.specter :refer :all :exclude [pred]]
            [fipp.edn :refer [pprint] :rename {pprint fipp}]))

(def TREE
  (recursive-path
   [] p
   (if-path coll?
            [ALL p]
            STAY)))

(defn expand
  "Pretty-printed macroexpansion with unqualified names."
  [f]
  (->> f
       macroexpand-all
      ;; (transform [TREE] (comp symbol name))
      fipp))

(defn deepcount
  "Counts levels of nesting in *evenly* nested collections."
  [coll]
  (loop [i 1
         coll coll]
    (if (coll? (first coll))
      (recur (inc i) (apply concat coll))
      i)))
