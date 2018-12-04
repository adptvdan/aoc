(ns aoc.core
  (:require [clojure.java.io :refer [resource]]
            [clojure.set :refer [difference intersection union]]
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

(defn day3-base []
  (let [coords (fn [x d]
                 (map #(Long. %)
                      (split (re-find (re-pattern (str "\\d+" d "\\d+")) x)
                             (re-pattern d))))
        idf (fn [x] (re-find #"#\d+" x))]
    (reduce
     (fn [acc row]
       (let [[x y] (coords row ",")
             [a b] (coords row "x")
             id (idf row)]
         (reduce
          #(update %1 %2 (fnil conj []) id)
          acc
          (for [i (range x (+ x a))
                j (range y (+ y b))]
            [i j]))))
     {}
     (read-raw-in 3))))

(defn day3p1 []
  (count (filter #(> (count %) 1) (vals (day3-base)))))

(defn day3p2 []
  (letfn [(f [c] (apply union (map set (filter #(c (count %) 1) (vals (day3-base))))))]
    (first (difference (f =) (f >)))))
