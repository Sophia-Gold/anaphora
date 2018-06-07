(ns anaphora.macros
  (:require [anaphora.util :refer :all]
            [anaphora.chain :refer :all]
            [com.rpl.specter :refer :all]
            [com.rpl.specter.zipper :refer :all] 
            [clojure.tools.analyzer.jvm :refer [analyze]]
            [fipp.edn :refer [pprint] :rename {pprint fipp}]))

(defmacro fn->
  "Converts bound variables from De Bruijn indices to curried univariate gensymed fns."  
  [x]
  (loop [x x
         i 9]
    (if (zero? i)
      x
      (let [pattern (re-pattern (str "%" i))
            fresh-var (gensym)] 
        (recur (transform (collect TREE symbol? (selected? NAME pattern))
                          #(if (not-empty %1)
                             (list `fn [fresh-var]
                                   (setval [TREE symbol? NAME pattern] (str fresh-var) %2))
                             %2)
                          x)
               (dec i))))))

(defmacro fn->>
  "Like `->>` but converts bound variables from De Bruijn indices to univariate gensymed fns."  
  [& x]
  (loop [x (macroexpand-1 (cons `->> x))
         i 9]
    (if (zero? i)
      x
      (let [pattern (str "%" i)
            fresh-var (gensym)]
        (recur (transform [SEQ-ZIP
                           (find-first #(= % (symbol pattern)))
                           UP
                           NODE
                           (transformed [TREE symbol? NAME (re-pattern pattern)]
                                        (fn [_] (str fresh-var)))]
                          #(list `fn [fresh-var] %)
                          x)
               (dec i))))))

;; (defmacro fn->>
;;   "Applies a series of forms over each level of a nested collection.
;;   Forms are applied from inside out, e.g. (`f` (`f` (`f` `form`)))." 
;;   [f x & forms]
;;   (let [nest #(let [arg (gensym)]
;;                 (list `fn [arg] (list f % arg)))]
;;     (loop [nests '()
;;            forms forms] 
;;       (if (not-empty (next forms))
;;         (let [form (first forms)
;;               threaded (if (seq? form)
;;                          (with-meta (nth (iterate nest form) (dec (count forms))) (meta form))
;;                          (nth (iterate nest form) (dec (count forms))))]
;;           (recur (cons nest threaded) (next forms)))
;;         (list `f (concat (list `comp (first forms)) nests) x)))))

(defmacro map->
  "Maps a series of forms over each level of a nested collection.
  Forms are applied from inside out, e.g. (map (map (map `f`))).
  All except for deepest (first in argument list) must be unary."
  [x & forms]
  (let [mapiter #(let [arg (gensym)]
                   (list `fn [arg] (list `map % arg)))]
    (loop [maps (list (nth (iterate mapiter (list `fn '[x] (list `map-indexed (first forms) 'x)))  
                           (- (count forms) 2)))  ;; specific to this example
           forms (next forms)] 
      (if (not-empty (next forms))
        (let [form (first forms)
              threaded (if (seq? form)
                         (with-meta (nth (iterate mapiter form) (dec (count forms))) (meta form))
                         (nth (iterate mapiter form) (dec (count forms))))]
          (recur (cons threaded maps) (next forms)))
        (list `map (concat (list `comp (first forms)) maps) x)))))

(defmacro map->>
  "Maps a series of forms over each level of a nested collection.
  Forms are applied from inside out, e.g. (map (map (map `f`))).
  All except for deepest (first in argument list) must be unary."
  [x & forms]
  (let [mapiter #(let [arg (gensym)]
                   (list `fn [arg] (list `map % arg)))]
    (loop [maps '()
           forms forms]
      (if (not-empty (next forms))
        (let [form (first forms)
              threaded (if (seq? form)
                         (with-meta (nth (iterate mapiter form) (dec (count forms)))  (meta form))
                         (nth (iterate mapiter form) (dec (count forms))))]
          (recur (cons threaded maps) (next forms)))
        (list `map (concat (list `comp (first forms)) maps) x)))))

(def chain-ast
  (fn
    [f g order]
    (let [f (nth (iterate add-dim f)
                 (dec (long (count (ffirst g)))))
          f' (diff-unmixed1 f order 1)
          g' (diff g order)] 
      (->> order
           partition-set
           (map (fn [p]
                  (mul (multi-compose (nth f' (dec (count p))) g)
                       (->> p
                            (map (fn [b] (->> b
                                             (map-indexed #(*' (long (Math/pow 10 %1)) 
                                                               %2))
                                             (reduce +')
                                             (get g'))))
                            (apply mul)))))
           (apply add)))))

(defn select-locals
  [ast] 
  (select (map-key-walker :locals) ast))

;; #(and (not= :locals (first %)) (not (coll? (second %))))]
(defn keep-locals
  [ast]
  (->> ast
       (setval [MAP-NODES
                ALL 
                (not-selected? FIRST (pred= :locals))
                (not-selected? FIRST (pred= :params))
                (not-selected? FIRST (pred= :args))
                (not-selected? FIRST (pred= :arglist))
                (not-selected? FIRST (pred= :arglists))
                (selected? LAST (complement map?))
                (selected? LAST (complement vector?))]
               NONE)
       (setval [MAP-NODES ALL (selected? FIRST (pred= :children))] NONE)))
