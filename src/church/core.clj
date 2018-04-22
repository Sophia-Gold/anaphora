(ns church.core
  (:require [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            [clojure.template :refer [apply-template]]
            [clojure.walk :refer [macroexpand-all]]
            [com.rpl.specter :refer :all]
            [com.positronic-solutions.pulley.cps :refer :all]))

(def TREE
  (recursive-path
   [] p
   (if-path coll?
            [ALL p]
            STAY)))

(defn expand [f]
  (->> f
       macroexpand-all
      (transform [TREE] (comp symbol name))
      pprint))

(defn church [n]
  (letfn [(succ [n] (fn [f] (fn [x] (f ((n f) x)))))]
    (loop [i 0
           ret (fn [f] (fn [x] x))]
      (if (= i n)
        ret
        (recur (inc i) (succ ret))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Church numerals:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro zero  [] `(fn [f] (fn [x] x)))
(defmacro one   [] `(fn [f] (fn [x] (f x))))
(defmacro two   [] `(fn [f] (fn [x] (f (f x)))))
(defmacro three [] `(fn [f] (fn [x] (f (f (f x))))))
(defmacro four  [] `(fn [f] (fn [x] (f (f (f (f x)))))))
(defmacro five  [] `(fn [f] (fn [x] (f (f (f (f (f x))))))))
(defmacro six   [] `(fn [f] (fn [x] (f (f (f (f (f (f x)))))))))
(defmacro seven [] `(fn [f] (fn [x] (f (f (f (f (f (f (f x))))))))))
(defmacro eight [] `(fn [f] (fn [x] (f (f (f (f (f (f (f (f x)))))))))))
(defmacro nine  [] `(fn [f] (fn [x] (f (f (f (f (f (f (f (f (f x))))))))))))

(defmacro succ [n] `(fn [f] (fn [x] (f (~n f) x))))

;; (defn succ   [n] (fn [f] (fn [x] (f ((n f) x)))))
(defn add  [m n] (fn [f] (fn [x] ((m f) ((n f) x)))))
(defn mult [m n] (fn [f] (fn [x] ((m (n f)) x))))
(defn pow  [m n] (fn [f] (fn [x] (((n m) f) x))))
;; (defn pred   [n] (fn [f] (fn [x] (((n (fn [g] (fn [h] (h (g f))))) (fn [u] x)) (fn [u] u)))))
(defn sub  [m n] (fn [f] (fn [x] ((((n pred) m) f) x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  ...as tagged literals:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defn zero  [f] #(%))
;; (defn one   [f] #(f %))
;; (defn two   [f] (f #(f %)))
;; (defn three [f] (f (f #(f %))))
;; (defn four  [f] (f (f (f #(f %)))))
;; (defn five  [f] (f (f (f (f #(f %))))))
;; (defn six   [f] (f (f (f (f (f #(f %)))))))
;; (defn seven [f] (f (f (f (f (f (f #(f %))))))))
;; (defn eight [f] (f (f (f (f (f (f (f #(f %)))))))))
;; (defn nine  [f] (f (f (f (f (f (f (f (f #(f %))))))))))

;; (defn succ   [n] (fn [f] (f #((n f) %))))
;; (defn add  [m n] (fn [f] ((m f) #((n f) %))))
;; (defn mult [m n] (fn [f] #((m (n f)) %)))
;; (defn pow  [m n] (fn [f] #(((n m) f) %)))
;; (defn pred   [n] (fn [f] (#((n (fn [g] (fn [h] (h (g f))))) %)) (fn [u] u)))
;; (defn sub  [m n] (fn [f] #((((n pred) m) f) %))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  ...as indexed literals:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defn zero  [f] #(%))
;; (defn one   [f] #(f %))
;; (defn two   [f] (f #(f %)))
;; (defn three [f] (f (f #(f %))))
;; (defn four  [f] (f (f (f #(f %)))))
;; (defn five  [f] (f (f (f (f #(f %))))))
;; (defn six   [f] (f (f (f (f (f #(f %)))))))
;; (defn seven [f] (f (f (f (f (f (f #(f %))))))))
;; (defn eight [f] (f (f (f (f (f (f (f #(f %)))))))))
;; (defn nine  [f] (f (f (f (f (f (f (f (f #(f %))))))))))

;; (defn succ   [n] #(%1 ((n %1) %2)))
;; (defn add  [m n] #((m %1) ((n %1) %2)))
;; (defn mult [m n] #((m (n %1)) %2))
;; (defn pow  [m n] #(((n m) %1) %2))
;; (defn pred   [n] #(((n (%4 (%3 %1)) %2)) %5)))
;; (defn sub  [m n] #((((n pred) m) %1) %2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  ...as *illegally* tagged literals:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defn pred [n] #(#((n #(% #(% %))) %) %))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Primitve and Church numeral factorials:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def factorial
((fn [f] (f f))
 (fn [f]
   (fn [n]
     (if (zero? n)
       1
       (* n ((f f) (dec n))))))))

(def church-factorial
  ((fn [f] (f f))
   (fn [f]
     (fn [n]
       (if (zero? (n inc 0))
         (fn [f x] (f x))
         (fn [f x] (n (fn [s] (((f f) (fn [f x] ((n (fn [g] (fn [h] (h (g f))))
                                                 (fn [u] x))
                                              (fn [a] a)))) f s)) x)))))))

(def church
  (fn [m]
    ((((fn [f] (f f))
       (fn [f]
         (fn [n]
           (if (zero? (n inc 0))
             (fn [f' x] (f' x))
             (fn [f' x] (n
                        (fn [s] (((f f)
                                 (fn [f x] ((n
                                            (fn [g]
                                              (fn [h]
                                                (h (g f))))
                                            (fn [u] x))
                                           (fn [a] a)))) f s)) x))))))
      (((fn [f] (f f)
          (fn [f]
            (fn [n']
            (if (zero? n')
              (fn [f x] x)
              (fn [f x]
                (((f f) (dec n')) f (f x))))))) m)) inc 0)))) 

(defn -main []
  )
