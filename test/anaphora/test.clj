(ns anaphora.test
  (:require [clojure.test :refer :all]
            [anaphora :refer :all]
            [anaphora.church :refer :all :exclude [pred pred' pred'']]))

(deftest chain-test
  (is (= (chain {[2] 1, [1] 5, [0] 7}
                {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7}
                2)
         {[1 1] 16, [1 0] 24, [0 1] 40, [0 0] 68})))

(deftest chain2-test
  (is (= (chain2 {[2] 1, [1] 5, [0] 7}
                 {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7}
                 2)
         {[1 1] 16, [1 0] 24, [0 1] 40, [0 0] 68})))

(deftest chain3-test
  (is (= (chain3 {[2] 1, [1] 5, [0] 7}
                 {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7}
                 2)
         {[1 1] 16, [1 0] 24, [0 1] 40, [0 0] 68})))

(deftest chain4-test
  (is (= (chain4 {[2] 1, [1] 5, [0] 7}
                 {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7}
                 2)
         {[1 1] 16, [1 0] 24, [0 1] 40, [0 0] 68})))

(deftest chain5-test
  (is (= (chain5 {[2] 1, [1] 5, [0] 7}
                 {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7}
                 2)
         {[1 1] 16, [1 0] 24, [0 1] 40, [0 0] 68})))

(deftest chain6-test
  (is (= (chain6 {[2] 1, [1] 5, [0] 7}
                 {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7}
                 2)
         {[1 1] 16, [1 0] 24, [0 1] 40, [0 0] 68})))

(deftest succ-test
  (= (cstr (succ (succ zero)))
     (cstr two)))

;; (deftest succ2-test
;;   (= (cstr (succ' (succ' zero')))
;;      (cstr two')))

(deftest succ3-test
  (= (cstr (succ'' (succ'' zero'')))
     (cstr two'')))

(deftest pred-test
  (= (cstr (pred (pred nine)))
     (cstr seven)))

;; (deftest pred2-test
;;   (= (cstr (pred' (pred' nine')))
;;      (cstr seven')))

(deftest pred3-test
  (= (cstr (pred'' (pred'' nine'')))
     (cstr seven'')))
