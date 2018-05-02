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

(defmacro try? [body]
  (try ~body
       (catch IllegalArgumentException e
         false)))

(defn nmap
  "Maps a series of functions over each level of a nested collection.
  Returns an indexed function literal."
  [coll & xforms]
  (loop [xforms (vec xforms)
         coll coll]
    (if (empty? xforms)
      coll
      (recur (pop xforms)
             (nth (iterate #(map (peek xforms) %) coll) (count xforms))))))

(defn deepcount [coll]
  (loop [i 0
         coll coll]
    (if (coll? coll)
      (recur (inc i) (apply concat coll))
      i)))

;; (map (fn [x] (map (fn [y] (map inc y))) x) [[[0 1 2]]])
;; (map (fn [x] (map (fn [y] (concat y)))) [[[0] 1] 2])

;; Luckily, we can alway solve that with macros :)
(defn chain3
  [f g order]
  (let [f (nth (iterate add-dim f)
               (dec (long (count (ffirst g)))))
        f' (diff-unmixed1 f order 1)
        g' (diff g order)
        xf1 #(*' (long (Math/pow 10 %1))
                 %2) 
        xf2 #(->> %2
                  (interleave (range))
                  (partition 2)
                  (map %1) 
                  (reduce +')
                  (get g'))
        xf3 #(->> %2
                  (map %1)
                  (apply mul)
                  (mul (multi-compose (nth f' (dec (count %2))) g)))]
    (apply add
           (nmap (partition-set order) xf1 xf2 xf3))))

(defn chain4
  "Only works with fork of Clojure allowing nested literals." 
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

(defn -main []
  )
