(ns aoc.core
  (:require [clojure.java.io :refer [resource]]
            [clojure.set :refer [intersection]]
            [clojure.string :refer [split]]))

(defn read-raw-in [i]
  (split (slurp (resource (str "d" i))) #"\n"))

(defn long-in [i]
  (map #(Long. %) (read-raw-in i)))

(defn day1p1 []
  (apply + (long-in 1)))

(defn day1p2 []
  (let [di (long-in 1)]
    (reduce
     #(if (%1 %2) (reduced %2) (conj %1 %2))
     #{}
     (reductions + (apply concat (repeat di))))))

(defn day2p1 []
  (let [xs (reduce
            (fn [acc x]
              (concat acc (distinct (filter #(< 1 % 4) (vals (frequencies x))))))
            []
            (read-raw-in 2))]
    (apply * (vals (frequencies xs)))))

(defn day2p2 []
  (let [xs (read-raw-in 2)]
    (first
     (for [a xs b xs
           :let [c (count (filter false? (map = a b)))]
           :when (= c 1)]
       (let [incl (intersection (set a) (set b))]
         (apply str (remove nil? (map incl a))))))))
