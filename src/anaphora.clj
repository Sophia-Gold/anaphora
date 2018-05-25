(ns anaphora
  (:require [anaphora.chain :refer :all]
            [anaphora.church :as c]
            [anaphora.macros :refer :all]
            [anaphora.util :refer :all]))
       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Predecessor function:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def pred (fn [n]
            (fn [f]
              (fn [x]
                (((n (fn [g]
                       (fn [h]
                         (h (g f)))))
                  (fn [u] x))
                 (fn [u] u))))))

(def pred' #(#(#(% #(% #(%1 %2))) %) %))

(def pred'' (fn-> (((%1 (%5 (%4 %2))) %3) %6)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Multivariate Faà di Bruno's formula:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
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

(defn chain2
  "Lambda lifting. Not quite what we're going for..."
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
