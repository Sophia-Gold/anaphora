(ns anaphora
  (:require [anaphora.chain :refer :all]
            [anaphora.church :as church]
            [anaphora.util :refer :all]
            [com.rpl.specter :refer :all :exclude [pred]]
            [com.rpl.specter.zipper :refer :all]
            [clojure.tools.analyzer :as ana]
            [clojure.tools.analyzer.jvm :refer [analyze]]
            [clojure.tools.analyzer.env :as env]))

(defn chain
  [f g order]
  (let [f (nth (iterate add-dim f)  ;; coerce `f` to dimensionality of `g`
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
         (apply add))))

;; (defn chain2
;;   "Doesn't work... De Bruijn indices fundamentally do not nest!"
;;   [f g order]
;;   (let [f (nth (iterate add-dim f)
;;                (dec (long (count (ffirst g)))))
;;         f' (diff-unmixed1 f order 1)
;;         g' (diff g order)] 
;;     (->> order
;;          partition-set 
;;          (map #(mul (multi-compose (nth f' (dec (count %1))) g)
;;                     (->> %1
;;                          (map (->> %2
;;                                    (map-indexed (*' (long (Math/pow 10 %4)) 
;;                                                     %3))
;;                                    (reduce +')
;;                                    (get g')))
;;                          (apply mul))))
;;          (apply add))))

(defn chain2
  "Easily solved with lambda lifting.
  But not quite what we're going for..."
  [f g order]
  (let [f (nth (iterate add-dim f)
               (dec (long (count (ffirst g)))))
        f' (diff-unmixed1 f order 1)
        g' (diff g order)
        xf1 #(*' (long (Math/pow 10 %1)) %2)
        xf2 #(->> %
                  (map-indexed xf1) 
                  (reduce +')
                  (get g'))
        xf3 #(->> %
                  (map xf2) 
                  (apply mul)
                  (mul (multi-compose (nth f' (dec (count %))) g)))]
    (->> order
         partition-set
         (map xf3)
         (apply add))))

;; (defn chain3
;;   "Idiomatic Clojure is to use transducers instead...
;;   ...but the order of composition conflicts when partially applied :("
;;   [f g order]
;;   (let [f (nth (iterate add-dim f)
;;                (dec (long (count (ffirst g)))))
;;         f' (diff-unmixed1 f order 1)
;;         g' (diff g order)
;;         xf1 #(*' (long (Math/pow 10 %1)) %2)
;;         xf2 #(->> %2
;;                   (map-indexed %1) 
;;                   (reduce +')
;;                   (get g'))
;;         xf3 #(->> %2
;;                   (map %1)
;;                   (apply mul)
;;                   (mul (multi-compose (nth f' (dec (count %2))) g)))]
;;     (->> order
;;          partition-set
;;          (sequence (partial xf3 (partial xf2 xf1)))
;;          (apply add)))) 

(defn chain3
  "A bit closer: lambda lifting with mapping factored out.
  Only works with fork of Clojure allowing nested literals."
  [f g order]
  (let [f (nth (iterate add-dim f)
               (dec (long (count (ffirst g)))))
        f' (diff-unmixed1 f order 1)
        g' (diff g order)
        x (partition-set order)
        xf1 #(*' (long (Math/pow 10 %1)) %2)  ;; finishing function 
        xf2 #(->> %
                  (reduce +')
                  (get g'))
        xf3 #(->> %
                  (apply mul)
                  (mul (multi-compose (nth f' (dec (count %))) g)))]
    (apply add
           (map
            (comp xf3
                  #(map xf2 %)
                  #(map #(map-indexed xf1 %) %))
            x))))

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

(defn chain4'
  "Instead of `partial`, compose nested lambdas with a threading macro: `map->`"
  [f g order]
  (let [f (nth (iterate add-dim f)
               (dec (long (count (ffirst g)))))
        f' (diff-unmixed1 f order 1)
        g' (diff g order)
        x (partition-set order)
        xf1 #(->> % 
                  (interleave (range))
                  (partition 2)
                  (map #(*' (long (Math/pow 10 (first %))) (second %)))
                  (reduce +')
                  (get g'))
        xf2 #(->> %
                  (apply mul)
                  (mul (multi-compose (nth f' (dec (count %))) g)))]
    (apply add 
           (map->> x
                   xf1
                   xf2))))

(defn chain4
  "Instead of `partial`, compose nested lambdas with a threading macro: `map->`"
  [f g order]
  (let [f (nth (iterate add-dim f)
               (dec (long (count (ffirst g)))))
        f' (diff-unmixed1 f order 1)
        g' (diff g order)
        x (partition-set order)
        xf1 #(*' (long (Math/pow 10 %1)) %2)  ;; finishing function
        xf2 #(->> %
                  (reduce +')
                  (get g'))
        xf3 #(->> %
                  (apply mul)
                  (mul (multi-compose (nth f' (dec (count %))) g)))]
    (apply add 
           (map-> x
                  xf1
                  xf2
                  xf3))))

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

;; (defn chain4''
;;   [f g order]
;;   (let [f (nth (iterate add-dim f)
;;                (dec (long (count (ffirst g)))))
;;         f' (diff-unmixed1 f order 1)
;;         g' (diff g order)
;;         x (partition-set order)
;;         xf1 #(->> % 
;;                   (interleave (range))
;;                   (partition 2)
;;                   (map #(*' (long (Math/pow 10 (first %))) (second %)))
;;                   (reduce +')
;;                   (get g'))
;;         xf2 #(->> %
;;                   (apply mul)
;;                   (mul (multi-compose (nth f' (dec (count %))) g)))]
;;     (apply add 
;;            (fn->> map x
;;                   xf1
;;                   xf2))))

(defn chain5
  "Only works with fork of Clojure allowing nested literals.
  Note that nested variables shadow one another...not ideal." 
  [f g order]
  (let [f (nth (iterate add-dim f)
               (dec (long (count (ffirst g)))))
        f' (diff-unmixed1 f order 1)
        g' (diff g order)] 
    (->> order
         partition-set
         (map #(->> %
                    (map #(->> %
                               (map-indexed #(*' (long (Math/pow 10 %1)) 
                                                 %2))
                               (reduce +')
                               (get g')))
                    (apply mul)
                    (mul (multi-compose (nth f' (dec (count %))) g))))
         (apply add))))

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

(defn chain6
  "Finally, *actual* De Bruijn indices!"
  [f g order]
  (let [f (nth (iterate add-dim f)
               (dec (long (count (ffirst g)))))
        f' (diff-unmixed1 f order 1)
        g' (diff g order)]
    (fn->> order
           partition-set
           (map (->> %1
                     (map (->> %2
                               (interleave (range))
                               (partition 2) 
                               (map (->> %3 ((fn [[idx v]] (*' (long (Math/pow 10 idx)) v))))) 
                               (reduce +')
                               (get g')))
                     (apply mul)
                     (mul (multi-compose (nth f' (dec (count %1))) g))))
           (apply add))))

(defn -main []
  )
