(ns anaphora
  (:require [anaphora.chain :refer :all]
            [anaphora.church :as church]
            [fipp.edn :refer [pprint]]
            [clojure.string :as str]
            [com.positronic-solutions.pulley.cps :refer :all]
            [clojure.tools.analyzer :as ana]
            [clojure.tools.analyzer.jvm :refer [analyze]]
            [clojure.tools.analyzer.env :as env]
            ;; [clojure.tools.decompiler :refer :all]))
            [no.disassemble :refer :all]))

;; (def chain
;;    (analyze
;;     '(fn [f g order]
;;        (let [f (nth (iterate add-dim f)  ;; coerce `f` to dimensionality of `g`
;;                     (dec (long (count (ffirst g)))))
;;             f' (diff-unmixed1 f order 1)
;;              g' (diff g order)]
;;          (->> order
;;               partition-set
;;               (map (fn [p]
;;                      (mul (multi-compose (nth f' (dec (count p))) g)
;;                           (->> p
;;                                (map (fn [b] (->> b
;;                                                 (map-indexed #(*' (long (Math/pow 10 %1)) 
;;                                                                   %2)) 
;;                                                 (reduce +')
;;                                            (get g'))))
;;                                (apply mul)))))
;;               (apply add))))))

(def chain
  (cps-fn
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
          (apply add)))))

(defn chain-test []
  (= (chain {[2] 1, [1] 5, [0] 7}
            {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7}
            2)
     {[1 1] 16, [1 0] 24, [0 1] 40, [0 0] 68}))

(defn stacks []
  (->> disassemble
       call-cc
       str/split-lines 
       (filter #(re-find #"Stack" %)) 
       (map (comp (fn [s] (map #(Long/parseLong (re-find #"\d+" %)) s))
                  #(str/split % #",")))
       (map #(zipmap [:stack :locals] %))
       (sort-by :stack)
       pprint))

(defn get-cc []
  (call-cc 
   (fn [cc]
     #(cc (conj [] %)))))

(defn read-cc []
  (-> (get-cc)
      disassemble-data 
      pprint))

(defn -main []
  )
