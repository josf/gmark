(ns gmark.parse
  (:require [clojure.string :as str]))


(defn sort-token-groups [token-pairs]
  (reverse (sort-by #(count (first %)) token-pairs)))



(defn match-by-length [string [token-begin token-end]]
  "Matches either token-begin or token-end against string. Matching is
  length based in that if a token is n chars long, we just match
  against the first n chars in string. End tokens match only if they
  occur at the end of the string."
  (let [same-toks (= token-begin token-end)]
    (cond
      (and (>= (count string) (count token-begin))
        (= token-begin (subs string 0 (count token-begin))))
      {:token token-begin :type (if same-toks :either :begin)}

      (and (>= (count (str/trim string)) (count token-end))
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

(defn drop-with-first [pred coll]
  (rest (drop-while (complement pred) coll)))

(defn structure [tokenized root token-map]
  (let [token (first tokenized)]
    (cond
      (not token)
      root

      (string? token)
      (structure
        (next tokenized)
        (assoc root
          :content (conj (:content root) token))
        token-map)

      (and (map? token) (:no-content (get token-map (:token token))))
      (structure
        (next tokenized)
        (assoc root
          :content (conj
                     (:content root)
                     {:tag  (:tag (get token-map (:token token)))
                      :attrs {}
                      :content []}))
        token-map)

      (and (map? token) (contains? token :token) (not (:no-content token)))
      (let [elem-type (get token-map  (:token token))
            closing-token (:closing-tag elem-type)]
        (structure
          (drop-with-first ; avoid the tokens that go to the new element
            #(and (map? %) (= closing-token (:token %)))
            (next tokenized))
          (assoc root                  ; same old root element but...
            :content
            (conj                      ;... we append...
              (:content root)
              (structure           ;... the new element that receives
                (take-while ; the tokens that precede the next closing tag
                  #(not
                     (and (map? %) (= closing-token (:token %))))
                  (next tokenized))
                {:tag (:tag elem-type)
                 :attrs {}
                 :content []}
                token-map)))
          token-map)))))
