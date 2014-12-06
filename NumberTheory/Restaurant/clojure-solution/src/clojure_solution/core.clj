(ns clojure-solution.core)
(require '[clojure.string :as string])

(defn parse-int [s]
  (Integer. s))

(defn parse-line [s]
  (map parse-int (string/split s #" ")))

(defn get-loaf-dimensions []
  (let [numLoaves (parse-int (read-line))]
    (map #(%1) (repeat numLoaves (comp parse-line read-line)))))

(def squares (map #(* % %) (range 1 1001)))

(defn min-number-of-slices [dims]
  (let [l              (first dims)
        b              (second dims)
        area           (* l b)]
    (letfn [(perfect-slice-dimension? [slice-area]
              (= 0 (+ (mod area slice-area)
                      (mod b (int (Math/sqrt slice-area)))
                      (mod l (int (Math/sqrt slice-area))))))]
      (let [largest-square (last (filter perfect-slice-dimension? (take (min l b) squares)))]
        (int (/ area largest-square))))))

(defn -main [& args] (dorun (map println (map min-number-of-slices (get-loaf-dimensions)))))

