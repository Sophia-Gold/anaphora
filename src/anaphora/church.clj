(ns anaphora.church
  (:require [clojure.walk :refer [macroexpand-all]]
            [com.rpl.specter :refer :all :exclude [pred]]
            [fipp.edn :refer [pprint] :rename {pprint fipp}]))

(def TREE
  (recursive-path
   [] p
   (if-path coll?
            [ALL p]
            STAY)))

(defn expand
  "Pretty-printed macroexpansion with unqualified names."
  [f]
  (->> f
       macroexpand-all
      (transform [TREE] (comp symbol name))
      fipp))

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

(defmacro zero  [] '(fn [f] (fn [x] x)))
(defmacro one   [] '(fn [f] (fn [x] (f x))))
(defmacro two   [] '(fn [f] (fn [x] (f (f x)))))
(defmacro three [] '(fn [f] (fn [x] (f (f (f x))))))
(defmacro four  [] '(fn [f] (fn [x] (f (f (f (f x)))))))
(defmacro five  [] '(fn [f] (fn [x] (f (f (f (f (f x))))))))
(defmacro six   [] '(fn [f] (fn [x] (f (f (f (f (f (f x)))))))))
(defmacro seven [] '(fn [f] (fn [x] (f (f (f (f (f (f (f x))))))))))
(defmacro eight [] '(fn [f] (fn [x] (f (f (f (f (f (f (f (f x)))))))))))
(defmacro nine  [] '(fn [f] (fn [x] (f (f (f (f (f (f (f (f (f x))))))))))))

(defmacro succ [n]   '(fn [f] (fn [x] (f (~n f) x))))
(defmacro add  [m n] '(fn [f] (fn [x] ((~m f) ((~n f) x)))))
(defmacro mult [m n] '(fn [f] (fn [x] ((~m (~n f)) x))))
(defmacro pow  [m n] '(fn [f] (fn [x] (((~n ~m) f) x))))
(defmacro pred   [n] '(fn [f] (fn [x] (((~n (fn [g] (fn [h] (h (g f))))) (fn [u] x)) (fn [u] u)))))
(defmacro sub  [m n] '(fn [f] (fn [x] ((((~n pred) ~m) f) x))))

(defmacro ctrue   []    '(fn [a] (fn [b] a)))
(defmacro cfalse  []    '(fn [a] (fn [b] b)))
(defmacro cand [p q]   '(~p (~q ~p)))
(defmacro cor  [p q]   '(~p (~p ~q)))
(defmacro cnot [p]     '(~p (cfalse (ctrue))))
(defmacro cxor [a b]   '(~a (cnot ~b) ~b))
(defmacro cif  [p a b] '(~p ~a ~b))

(defmacro czero? [n]   '(~n (fn [x] cfalse) ctrue))
(defmacro leq?   [m n] '(czero? (minus ~m ~n)))
(defmacro eq?    [m n] '(cand (leq ~m ~n) (leq ~n ~m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  ...as tagged literals:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro zero'  [] '(fn [f] #(%)))
(defmacro one'   [] '(fn [f] #(f %)))
(defmacro two'   [] '(fn [f] #(f (f %))))
(defmacro three' [] '(fn [f] #(f (f (f %)))))
(defmacro four'  [] '(fn [f] #(f (f (f (f %))))))
(defmacro five'  [] '(fn [f] #(f (f (f (f (f %)))))))
(defmacro six'   [] '(fn [f] #(f (f (f (f (f (f %))))))))
(defmacro seven' [] '(fn [f] #(f (f (f (f (f (f (f %)))))))))
(defmacro eight' [] '(fn [f] #(f (f (f (f (f (f (f (f %))))))))))
(defmacro nine'  [] '(fn [f] #(f (f (f (f (f (f (f (f (f %)))))))))))

(defmacro succ'   [n] '(fn [f] #(f ((~n f) %))))
(defmacro add'  [m n] '(fn [f] #((~m f) ((~n f) %))))
(defmacro mult' [m n] '(fn [f] #((~m (~n f)) %)))
(defmacro pow'  [m n] '(fn [f] #(((~n ~m) f) %)))
(defmacro pred'   [n] '(fn [f] (#((~n (fn [g] (fn [h] (h (g f))))) %)) #(%)))
(defmacro sub'  [m n] '(fn [f] #((((~n pred) ~m) f) %)))

(defmacro ctrue'  []  '(fn [a] #(a)))
(defmacro cfalse' []  '(fn [a] #(%)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  ...as indexed literals:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro zero''  [] '#(%))
(defmacro one''   [] '#(%1 %2))
(defmacro two''   [] '#(%1 (%1 %2)))
(defmacro three'' [] '#(%1 (%1 (%1 %2))))
(defmacro four''  [] '#(%1 (%1 (%1 (%1 %2)))))
(defmacro five''  [] '#(%1 (%1 (%1 (%1 (%1 %2))))))
(defmacro six''   [] '#(%1 (%1 (%1 (%1 (%1 (%1 %2)))))))
(defmacro seven'' [] '#(%1 (%1 (%1 (%1 (%1 (%1 (%1 %2))))))))
(defmacro eight'' [] '#(%1 (%1 (%1 (%1 (%1 (%1 (%1 (%1 %2)))))))))
(defmacro nine''  [] '#(%1 (%1 (%1 (%1 (%1 (%1 (%1 (%1 (%1 %2))))))))))

(defmacro succ''   [n] '#(%1 ((~n %1) %2)))
(defmacro add''  [m n] '#((~m %1) ((~n %1) %2)))
(defmacro mult'' [m n] '#((~m (~n %1)) %2))
(defmacro pow''  [m n] '#(((~n ~m) %1) %2))
(defmacro pred'' [n]   '#(((~n (%4 (%3 %1))) %2) (%5)))
(defmacro sub''  [m n] '#((((~n pred) ~m) %1) %2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  ...as *illegally* tagged literals:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defmacro pred [n] '#(#((~n #(% #(% %))) %) %))

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
         acc '(one)]
    (expand
     '(cif (expand '(czero? (cnt)))
           (expand '(acc))
           (recur '(pred (cnt))
                  (expand '(mul (cnt) (acc))))))))
