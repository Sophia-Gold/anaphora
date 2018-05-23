(ns anaphora.church
  (:require [anaphora.util :refer :all]
            [com.rpl.specter :refer :all :exclude [pred]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Syntactically equivalent string conversion:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(def cstr  (fn [f] ((f (fn [n] (format "f(%s)" n))) "n")))
(def cstr' #((%1 (format "f(%s)" %2)) "n"))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Church numerals:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def zero  (fn [f] (fn [x] x)))
;; (def one   (fn [f] (f (fn [x] x))))
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
(def cor    (fn [p q] (p (p q))))
(def cnot   (fn [p] (p (cfalse (ctrue)))))
(def cxor   (fn [a b] (a (cnot b) b)))
(def cif    (fn [p a b] (p a b)))

(def czero? (fn [n] (n (fn [x] cfalse) ctrue)))
(def leq?   (fn [m] (fn [n] (czero? (sub m n)))))
(def eq?    (fn [m] (fn [n] (cand (leq? m n) (leq? n m)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  ...as tagged literal (requires fork of Clojure allowing nested literals):
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def zero' #(let [x %] #(str %)))
(def one'  #(let [x %] #(x %)))
;; (def zero'  #(%))
;; (def one'   #(% #(%)))
(def two'   #(% (% #(%))))
(def three' #(% (% (% #(%)))))
(def four'  #(% (% (% (% #(%))))))
(def five'  #(% (% (% (% (% #(%)))))))
(def six'   #(% (% (% (% (% (% #(%))))))))
(def seven' #(% (% (% (% (% (% (% #(%)))))))))
(def eight' #(% (% (% (% (% (% (% (% #(%))))))))))
(def nine'  #(% (% (% (% (% (% (% (% (% #(%)))))))))))

(def succ' #(% ((#(%) %) #(%))))
(def add'  #((#(%) %) ((#(%) %) #(%))))
(def mul'  #((#(%) (#(%) %)) #(%)))
(def pow'  #(((#(%) #(%)) %) #(%)))
(def pred' #(#(#(% #(% #(% %))) %)))
(def sub'  #((((#(%) pred) #(%)) %) #(%)))

(def ctrue'  #(%))
(def cfalse' '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  ...as indexed literals:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(def zero''  (fn-> %1))                ;; still prints as 1
(def one''   (fn-> (%1 %2)))
(def two''   (fn-> (%1 (%1 %2))))
(def three'' (fn-> (%1 (%1 (%1 %2)))))
(def four''  (fn-> (%1 (%1 (%1 (%1 %2))))))
(def five''  (fn-> (%1 (%1 (%1 (%1 (%1 %2)))))))
(def six''   (fn-> (%1 (%1 (%1 (%1 (%1 (%1 %2))))))))
(def seven'' (fn-> (%1 (%1 (%1 (%1 (%1 (%1 (%1 %2)))))))))
(def eight'' (fn-> (%1 (%1 (%1 (%1 (%1 (%1 (%1 (%1 %2))))))))))
(def nine''  (fn-> (%1 (%1 (%1 (%1 (%1 (%1 (%1 (%1 (%1 %2)))))))))))

(def succ'' (fn-> (%2 ((%1 %2) %3))))
(def add''  (fn-> ((%1 %3) ((%2 %3) %4))))
(def mul''  (fn-> ((%1 (%2 %3)) %4)))
(def pow''  (fn-> (((%1 %2) %3) %4)))
(def pred'' (fn-> (((%1 (%5 (%4 %2))) (%3)) %6)))
(def sub''  (fn-> ((((%2 pred) %1) %3) %4)))

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
    (cif (cstr (czero? cnt))
         (cstr acc)
         (church-factorial (pred cnt)
                           (mul cnt acc)))))
