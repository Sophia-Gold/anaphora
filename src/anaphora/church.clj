(ns anaphora.church
  (:require [anaphora.util :refer :all]))

(def print-church (fn [f] ((f (fn [n] (format "f(%s)" n))) "n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Church numerals:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def zero  (fn [f] (fn [x] x)))
(def one   (fn [f] (fn [x] (f x))))
(def two   (fn [f] (fn [x] (f (f x)))))
(def three (fn [f] (fn [x] (f (f (f x))))))
(def four  (fn [f] (fn [x] (f (f (f (f x)))))))
(def five  (fn [f] (fn [x] (f (f (f (f (f x))))))))
(def six   (fn [f] (fn [x] (f (f (f (f (f (f x)))))))))
(def seven (fn [f] (fn [x] (f (f (f (f (f (f (f x))))))))))
(def eight (fn [f] (fn [x] (f (f (f (f (f (f (f (f x)))))))))))
(def nine  (fn [f] (fn [x] (f (f (f (f (f (f (f (f (f x))))))))))))

(def succ (fn [n] (fn [f] (fn [x] (f ((n f) x))))))
(def add  (fn [m n] (fn [f] (fn [x] ((m f) ((n f) x))))))
(def mul  (fn [m n] (fn [f] (fn [x] ((m (n f)) x)))))
(def pow  (fn [m n] (fn [f] (fn [x] (((n m) f) x)))))
(def pred (fn   [n] (fn [f] (fn [x] (((n (fn [g] (fn [h] (h (g f))))) (fn [u] x)) (fn [u] u))))))
(def sub  (fn [m n] (fn [f] (fn [x] ((((n pred) m) f) x)))))

(def ctrue  (fn [a] (fn [b] a)))
(def cfalse (fn [a] (fn [b] b)))
(def cand   (fn [p q] (p (q p))))
(def cor    (fn [p q]   (p (p q))))
(def cnot   (fn [p]     (p (cfalse (ctrue)))))
(def cxor   (fn [a b]   (a (cnot b) b)))
(def cif    (fn [p a b] (p a b)))

(def czero? (fn [n] (n (fn [x] cfalse) ctrue)))
(def leq?   (fn [m n] (czero? (sub m n))))
(def eq?    (fn [m n] (cand (leq? m n) (leq? n m))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  ...as tagged literals:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def zero'   (fn [f] #(%)))
(def one'    (fn [f] #(f %)))
(def two'    (fn [f] #(f (f %))))
(def three'  (fn [f] #(f (f (f %)))))
(def four'   (fn [f] #(f (f (f (f %))))))
(def five'   (fn [f] #(f (f (f (f (f %)))))))
(def six'    (fn [f] #(f (f (f (f (f (f %))))))))
(def seven'  (fn [f] #(f (f (f (f (f (f (f %)))))))))
(def eight'  (fn [f] #(f (f (f (f (f (f (f (f %))))))))))
(def nine'   (fn [f] #(f (f (f (f (f (f (f (f (f %)))))))))))

(def succ' (fn [n] (fn [f] #(f ((n f) %)))))
(def add'  (fn [m n] (fn [f] #((m f) ((n f) %)))))
(def mul'  (fn [m n] (fn [f] #((m (n f)) %))))
(def pow'  (fn [m n] (fn [f] #(((n m) f) %))))
(def pred' (fn [n] (fn [f] (#((n (fn [g] (fn [h] (h (g f))))) %)) #(%))))
(def sub'  (fn [m n] (fn [f] #((((n pred) m) f) %))))

(def ctrue'  (fn [a] #(a)))
(def cfalse' (fn [a] #(%)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  ...as indexed literals:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def zero'   #(%))
(def one'    #(%1 %2))
(def two'    #(%1 (%1 %2)))
(def three'  #(%1 (%1 (%1 %2))))
(def four'   #(%1 (%1 (%1 (%1 %2)))))
(def five'   #(%1 (%1 (%1 (%1 (%1 %2))))))
(def six'    #(%1 (%1 (%1 (%1 (%1 (%1 %2)))))))
(def seven'  #(%1 (%1 (%1 (%1 (%1 (%1 (%1 %2))))))))
(def eight'  #(%1 (%1 (%1 (%1 (%1 (%1 (%1 (%1 %2)))))))))
(def nine'   #(%1 (%1 (%1 (%1 (%1 (%1 (%1 (%1 (%1 %2))))))))))

(def succ' #(%2 ((%1 %2) %2)))
(def add'  #((%2 %3) ((%1 %3) %4)))
(def mul'  #((%1 (%2 %3)) %4))
(def pow'  #(((%1 %1) %3) %4))
(def pred' #(((%1 (%5 (%4 %2))) %3) (%6)))
(def sub'  #((((%2 pred) %1) %3) %4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  ...as *illegally* tagged literals:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def pred-literal #(#(#(% #(% #(% %))) %) %))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Primitve and Church numeral factorials:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn factorial [n]
  (loop [cnt n
         acc 1]
    (if (zero? cnt)
      acc
      (recur (dec cnt)
             (* acc cnt))))) 

(defn church-factorial [n]
  (loop [cnt n
         acc one]
    (cif (print-church (czero? cnt))
         (print-church acc)
         (church-factorial (pred cnt)
                           (mul cnt acc)))))
