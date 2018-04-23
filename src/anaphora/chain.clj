(ns anaphora.chain
  (:require [clojure.data.avl :refer [sorted-map-by]]
            [clojure.data.int-map :as i]
            [clojure.math.combinatorics :as combo]
            [com.rpl.specter :refer :all]))

(defn add-dim
  "Projects a function into the next higher dimension
  by appending zeros to tuples of variables."
  [poly]
  (transform [MAP-KEYS] #(conj % 0) poly))

(defn denull
  "Removes terms with zero coefficients."
  [poly]
   (setval [MAP-VALS #(= % 0)] NONE poly))

(defn partition-set
  [n]
  (->> (range 1 (inc n))
       combo/partitions))

(defn grevlex
  "Comparator for tuple-keyed sorted-maps.
   Uses graded reverse lexicographic ordering
   for efficiency in computing Gröbner bases."
  [term1 term2]
  (let [grade1 (long (reduce +' term1))
        grade2 (long (reduce +' term2))
        comp (- grade2 grade1)] ;; total degree
    (if (not= 0 comp)
      comp
      (loop [term1 term1
             term2 term2]
        (if (empty? term1)
          0
           (let [grade1 (long (last term1))
                 grade2 (long (last term2))
                 comp (- grade1 grade2)] ;; differs from grlex because terms are flipped from above
             (if (not= 0 comp)
               comp
               (recur (pop term1)
                      (pop term2)))))))))

(defn add
  "Polynomial addition.
   Variadic, nullary version returns transducer."
  ([]  ;; init
    (fn
      ([poly] poly)
      ([poly1 poly2] (add poly1 poly2))
      ([poly1 poly2 & more] (reduce add (add poly1 poly2) more))))
  ([poly] poly)  ;; completion 
  ([poly1 poly2]  ;; step
   (denull
    (into (sorted-map-by grevlex)
          (merge-with +' poly1 poly2))))
  ([poly1 poly2 & more]
   (reduce add (add poly1 poly2) more)))

(defn scale [poly scalar]
  (->> poly
       (map #(update % 1 *' scalar))
       (into {})
       (denull)))

(defn mul
  "Polynomial multiplication.
   Variadic, nullary version returns transducer."
  ([]  ;; init
   (fn
     ([poly] poly)
     ([poly1 poly2] (mul poly1 poly2))
     ([poly1 poly2 & more] (reduce mul (mul poly1 poly2) more))))
  ([poly] poly)  ;; completion
  ([poly1 poly2]  ;; step
   (let [product (atom (transient (sorted-map-by grevlex)))]
     (doall  ;; `for` is lazy so must to be forced for side-effects 
      (for [term1 poly1
            term2 poly2
            :let [vars (mapv +' (key term1) (key term2))
                  coeff (* (long (val term1)) (long (val term2)))]]
        (if (get @product vars)  ;; `contains?` is broken until data.avl is updated for 1.9
          (swap! product assoc! vars (+ (long (get @product vars)) coeff))
          (swap! product assoc! vars coeff))))
     (->> product
          (deref)
          (persistent!)
          (denull))))
  ([poly1 poly2 & more]
   (reduce mul (mul poly1 poly2) more)))

(defn pow
  "Raises polynomial to exponent."
  ([poly ^long exp]
   {:pre [(>= exp 0)]}
   (cond
     (> exp 1) (->> poly
                    (repeat exp)
                    (apply mul))
     (= exp 1) poly
     (zero? exp) {(into (vector) (repeat (count (ffirst poly)) 0)) 1})))

(defn diff-unmixed
  "Same as `diff`, but computes only unmixed partials.
   Returns an int-map keyed by the differentiated variable
   containing ordered sequences of its unmixed partials."
  [poly ^long order]
  (let [tape (atom (transient (i/int-map)))
        dims (count (ffirst poly))]
    (letfn [(partial-diff [poly ^long idx]
              (loop [partial poly
                     n 0]
                (when (< n order)
                  (let [next-partial (transform [ALL] (fn [[k v]]
                                                        (let [var (long (nth k idx))]
                                                          (when (not (zero? var))
                                                            [(update k idx dec)
                                                             (* v var)])))
                                                partial)]
                    (swap! tape i/update! (inc idx) conj next-partial)
                    (recur next-partial (inc n))))))]
      (run! #(swap! tape assoc! % (vector)) (range 1 (inc dims)))
      (run! #(partial-diff poly %) (range dims))
      (persistent! @tape))))

(defn diff
  "Computes all partial derivates of a function up to a given order.
   Functions are represented as sorted-maps of monomials in graded 
   reverse lexicographic order with tuples of exponents as keys and 
   corresponding coefficients as values. Returns a map ('tape') with
   integer keys where number of digits represents order and least
   significant digits represent differentiated variables. Input is
   limited to functions of at most 9 variables."
  [poly ^long order]
  (let [tape (atom (transient (i/int-map)))
        dims (count (ffirst poly))]
    (letfn [(partial-diff [poly ^long key ^long idx]
              (let [partial (transform [ALL] (fn [[k v]]
                                               (let [var (long (nth k idx))]
                                                 (when (not (zero? var))
                                                   [(update k idx dec)
                                                    (* v var)])))
                                       poly)]
                (swap! tape assoc! key partial)
                (list partial key)))
             (diff-loop [poly ^long n]
               (when (< n order)
                 (run! #(diff-loop (partial-diff (first poly)
                                                 (+ (* 10 (long (second poly)))
                                                    (inc (long %)))
                                                %)
                                   (inc n))
                       (range dims))))]
       (diff-loop (list poly 0) 0)
       (persistent! @tape))))

(defn diff-unmixed1
  "Same as `diff`, but computes only unmixed partials.
   Returns an ordered sequence of unmixed partials
   of the variable at the given index."
  [poly ^long order ^long idx]
  (let [idx (dec idx)]  ;; tuples of exponents are ordered from 1...my bad, Dijkstra
    (letfn [(partial-diff [poly]
              (transform [ALL] (fn [[k v]]
                                 (let [var (long (nth k idx))]
                                   (when (not (zero? var))
                                     [(update k idx dec)
                                      (* v var)])))
                         poly))]
      (->> poly
           (iterate partial-diff)
           (drop 1)
           (take order)
           (into (vector))))))

(defn multi-compose
  "Functional composition.
   Substitutes `g` for all variables in `f`.
   `f` and `g` do *not* have to have the same arity.
   Variadic, unary version returns transducer."
  ([]  ;; init
   (fn
     ([f] f)
     ([f g] (multi-compose f g))
     ([f g & more] (reduce multi-compose (multi-compose f g) more))))
  ([f] f)  ;; completion
  ([f g]  ;; step
   (let [const (into (vector) (repeat (count (ffirst f)) 0))] ;; dims
     (->> f
          (#(dissoc % const)) ;; remove constant term from `f` (idempotent if not present)
          (mapcat (fn [[vars coeff]]
                    (map (fn [v]
                           (if (zero? v)
                             {}
                             (scale (pow g v) coeff)))
                         vars)))
          (cons {const (get f const)})  ;; add constant term from `f` (idempotent if not present)
          (apply add)
          (into (sorted-map-by grevlex)))))
  ([f g & more]
   (reduce multi-compose (multi-compose f g) more)))

;; (defn chain
;;   "Higher-order chain rule using Faà di Bruno's formula.
;;    Works over multiple variables in `g` using the \"collapsing partitions\" 
;;    technique from Michael Hardy's \"Combinatorics of Partial Derivatives.\"" 
;;   [f g order]
;;   (let [f (nth (iterate add-dim f)  ;; coerce `f` to dimensionality of `g`
;;                (dec (long (count (ffirst g)))))
;;         f' (diff-unmixed1 f order 1)
;;         g' (diff g order)]
;;     (->> order
;;          partition-set
;;          (map (fn [p]
;;                 (mul (multi-compose (nth f' (dec (count p))) g)
;;                      (->> p
;;                           (map (fn [b] (->> b
;;                                            (map-indexed #(*' (long (Math/pow 10 %1)) 
;;                                                              %2)) 
;;                                            (reduce +')
;;                                             (get g'))))
;;                           (apply mul)))))
;;          (apply add))))
