(ns anaphora
  (:require [anaphora.chain :refer :all] 
            [anaphora.church :as church]
            [anaphora.util :refer :all]
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

;;  Doesn't work... De Bruijn indices fundamentally do not nest!
;; (defn chain2
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

;; easily solved with lambda lifting, but not quite what we're going for...
(defn chain2
  [f g order]
  (let [f (nth (iterate add-dim f)
               (dec (long (count (ffirst g)))))
        f' (diff-unmixed1 f order 1)
        g' (diff g order)
        xf1 #(*' (long (Math/pow 10 (first %))) 
                 (second %))
        xf2 #(->> %
                  (interleave (range))
                  (partition 2)
                  (map xf1) 
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

;; Idiomatic Clojure is to use transducers instead...
;; ...but the order of composition conflicts when partially applied :(
;; (defn chain2
;;   [f g order]
;;   (let [f (nth (iterate add-dim f)
;;                (dec (long (count (ffirst g)))))
;;         f' (diff-unmixed1 f order 1)
;;         g' (diff g order)
;;         xf1 #(*' (long (Math/pow 10 (first %)))
;;                  (second %)) 
;;         xf2 #(->> %2
;;                   (interleave (range))
;;                   (partition 2)
;;                   (map %) 
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

(defmacro map->
  "Maps a series of forms over each level of a nested collection.
  Forms are applied from inside out, e.g. (map `f` (map `f` (map `f`))).
  All except for deepest (first in argument list) must be unary."
  [x & forms]
  (loop [x x
         forms forms]
    (if (not-empty (next forms))
      (let [form (macroexpand (first forms))
            threaded (if (seq? form)
                       (with-meta `(->> ~x (map ~(first form)) ~@(next form)) (meta form))
                       (list `map form x))]
        (recur threaded (next forms)))
      (list `map-indexed (first forms) x))))  ;; generalize: s/`map-indexed`/`map`

;; Luckily, we can alway solve that with macros :)
(defn chain3
  [f g order]
  (let [f (nth (iterate add-dim f)
               (dec (long (count (ffirst g)))))
        f' (diff-unmixed1 f order 1)
        g' (diff g order)
        x (partition-set order)
        xf1 (->> (apply mul)
                 (mul (multi-compose (nth f' (dec (count x))) g)))
        xf2 (->> (reduce +')
                 (get g'))
        xf3 #(*' (long (Math/pow 10 %1)) %2)]  ;; finishing function
    (apply add
           (expand
            '(map-> x
                    xf3
                    xf2
                    xf1)))))

(defn chain4
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

;; (defn chain5
;;   "*Actual* De Bruijn indices...now write a macro to make this work!" 
;;   [f g order]
;;   (let [f (nth (iterate add-dim f)
;;                (dec (long (count (ffirst g)))))
;;         f' (diff-unmixed1 f order 1)
;;         g' (diff g order)] 
;;     (->> order
;;          partition-set
;;          (map #(->> %1
;;                     (map #(->> %2
;;                                (map-indexed #(*' (long (Math/pow 10 %3)) 
;;                                                  %4))
;;                                (reduce +')
;;                                (get g')))
;;                     (apply mul)
;;                     (mul (multi-compose (nth f' (dec (count %1))) g))))
;;          (apply add))))
  
(defn -main []
  )
