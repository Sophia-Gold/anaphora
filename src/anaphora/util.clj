(ns anaphora.util
  (:require [clojure.walk :refer [macroexpand-all]]
            [com.rpl.specter :refer :all]
            [fipp.edn :refer [pprint] :rename {pprint fipp}]))

(def TREE
  (recursive-path
   [] p
   (if-path coll?
            [ALL p]
            STAY)))

(def MAP-TREE
  (recursive-path
   [] p
   (if-path map?
            [ALL p]
            STAY)))

(def MAP-NODES
  (recursive-path
   [] p
   (cond-path map? (stay-then-continue MAP-VALS p)
              coll? [ALL p])))

;; (def MAP-NODES
;;   (recursive-path [] p
;;    (cond-path map? (stay-then-continue MAP-VALS p)
;;               coll? [(compact ALL p)])))

(def map-key-walker
  (recursive-path
   [akey] p
   (cond-path map? [ALL (if-path [FIRST #(= % akey)]
                                 LAST
                                 [LAST p])]
              vector? [ALL (if-path map?
                                    [ALL (if-path [FIRST #(= % akey)]
                                                  LAST 
                                                  [LAST p])])])))

;; (def filter-keys
;;   (recursive-path
;;    [k] p
;;    (cond-path map? (stay-then-continue
;;                     ALL
;;                     (not-selected? (pred= k))
;;                     LAST
;;                     p)
;;               coll? [ALL p])))

(def filter-keys
  (recursive-path
   [k] p
   (cond-path map? [ALL (cond-path [FIRST (pred k)] STOP
                                   [LAST (pred (complement coll?))] STAY
                                   :else p)]
              coll? [ALL p])))

(defn expand
  "Pretty-printed macroexpansion with unqualified names."
  [f]
  (->> f
       macroexpand-all
      fipp))

(defn deepcount
  "Counts levels of nesting in *evenly* nested collections."
  [coll]
  (loop [i 1
         coll coll]
    (if (coll? (first coll))
      (recur (inc i) (apply concat coll))
      i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Currying:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn partial+
  "Takes a function f and fewer than the normal arguments to f, and
  returns a fn that takes a variable number of additional args. When
  called, the returned function calls f with args + additional args.
  differs from the core version in that it works on just one argument."
  {:added "1.0"}
  ([f] f)
  ([f arg1]
   (fn [& args] (apply f arg1 args)))
  ([f arg1 arg2]
   (fn [& args] (apply f arg1 arg2 args)))
  ([f arg1 arg2 arg3]
   (fn [& args] (apply f arg1 arg2 arg3 args)))
  ([f arg1 arg2 arg3 & more]
   (fn [& args] (apply f arg1 arg2 arg3 (concat more args)))))

(defn curry**
  [number-of-args f]
  (fn
    ([& args]
       (let [number-of-inputs (count args)]
	 (if (= number-of-inputs number-of-args)
	   (apply f args)
	   (curry** (- number-of-args number-of-inputs)
		    (apply (partial+ partial+ f) args)))))))

(def curry* (curry** 2 curry**))
