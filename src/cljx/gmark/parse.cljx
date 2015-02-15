(ns gmark.parse
  (:require [clojure.string :as str]))


(defn sort-token-groups [token-pairs]
  (reverse (sort-by #(count (first %)) token-pairs)))

(defn match-by-length [string [token-begin token-end]]
  (let [same-toks (= token-begin token-end)]
    (cond
      (and (>= (count string) (count token-begin))
        (= token-begin (subs string 0 (count token-begin))))
      {:token token-begin :type (if same-toks :either :begin)}

      (and (>= (count string) (count token-end))
        (= token-end (subs string 0 (count token-end))))
      {:token token-end :type :end}
      
      true
      nil)))

(defn char-to-accum [ch accum]
  (if (empty? accum)
    (vector (str ch))
    (let [the-last (last accum)]
      (if (string? the-last)
        (conj (vec (butlast accum)) (str the-last ch))
        (conj accum (str ch))))))

(defn tokenize [string tokens]
  "tokens is a list of vectors, each containing start and an end
  string. Both must be present, even if start and end tags are
  identical."
  (let [tokes (sort-token-groups tokens)]
    (loop [accum []
           s string]
      (if (empty? s)
        accum
        (if-let [match (first (keep #(match-by-length s %) tokes))]
          (recur (conj accum match) (subs s (count (:token match))))
          (recur (char-to-accum (first s) accum) (subs s 1)))))))
