(ns anaphora
  (:require [anaphora.chain :refer :all]
            [anaphora.church :as church]
            [fipp.edn :refer [pprint] :rename {pprint fipp}]
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

;; ;; easily solved with lambda lifting, but not quite what we're going for...
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

;; idiomatic Clojure is to use transducers instead
(defn chain3
  [f g order]
  (let [f (nth (iterate add-dim f)
               (dec (long (count (ffirst g)))))
        f' (diff-unmixed1 f order 1)
        g' (diff g order)
        xf1 #(*' (long (Math/pow 10 (first %)))
                 (second %)) 
        xf2 #(->> %2
                  (interleave (range))
                  (partition 2)
                  (map %) 
                  (reduce +')
                  (get g'))
        xf3 #(->> %2
                  (map %1)
                  (apply mul)
                  (mul (multi-compose (nth f' (dec (count %2))) g)))]
    (->> order
         partition-set
         (sequence (partial xf3 (partial xf2 xf1)))
         (apply add)))) 

(defn chain4
  "only works with fork of Clojure allowing nested literals" 
  [f g order]
  (let [f (nth (iterate add-dim f)
               (dec (long (count (ffirst g)))))
        f' (diff-unmixed1 f order 1)
        g' (diff g order)] 
    (->> order
         partition-set
         (map #(mul (multi-compose (nth f' (dec (count %))) g)
                    (->> %
                         (map #(->> %
                                    (map-indexed #(*' (long (Math/pow 10 %1)) 
                                                      %2))
                                    (reduce +')
                                    (get g')))
                         (apply mul)))
              (apply add)))))
 
(defn chain-test []
  (= (chain3 {[2] 1, [1] 5, [0] 7}
             {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7}
             2)
     {[1 1] 16, [1 0] 24, [0 1] 40, [0 0] 68}))

(defn -main []
  )
