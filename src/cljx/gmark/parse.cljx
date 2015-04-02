(ns gmark.parse
  (:require [clojure.string :as str]))


(defn sort-token-groups [token-pairs]
  (reverse (sort-by #(count (first %)) token-pairs)))


;;; token-map structure

;;; Basic config:
;;; 
;;; { opening-tag
;;;  {:tag tag-keyword :closing-tag closing-tag}}
;;; 
;;;  opening-tag and closing-tags are strings, ie. "{{" and "}}"
;;; 

(defn tokens-from-token-map [token-map]
  "Given the token-map (used by the structure function), return a
  vector of start/end pairs for tokenizing"
  (mapv
    (fn [[start-token info]]
      [start-token
       (if (:no-content info)
         start-token
         (:closing-tag info))])
    token-map))


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


(defn parse-chunk [text token-map root]
  (structure
    (tokenize text (tokens-from-token-map token-map))
    root
    token-map))

(defn pre-clean-regex [reg-str]
  "Escape regex characters"
  (str/replace reg-str #"([.\*])" "\\\\$1"))

(defn chunking-regex [line-starts]
  (re-pattern
    (apply str
      (interpose
        "|"
        (conj
          (map #(str "(?=\\n" (pre-clean-regex %) "\\s+)") line-starts)
          "\n\n")))))

(defn chunk-group-tokenize [text line-starts]
  "line-starts is a list of possible line beginning marker
  strings. With an empty list, defaults to '\n\n', meaning simple
  paragraphs."
  (map str/trim
   (if (empty? line-starts)
     (str/split text #"\n\n")
     (str/split text (chunking-regex line-starts)))))


(defn identify-chunk-type [chunk sorted-line-end-pairs]
  (first
    (keep
      (fn [[line-end elem-kw]]
        (when
         (and
           (> (count chunk) (count line-end))
           (=
             (str line-end " ")
             (subs chunk 0 (inc (count line-end)))))
         elem-kw))
      sorted-line-end-pairs)))

(defn parse-chunk-group [parent text line-starts-elems token-map]
  "line-starts-elems is a map of line-start strings to element type
  keywords."
  (let [sorted-line-end-pairs (sort-token-groups (seq line-starts-elems))]
   (assoc parent
     :content
     (mapv

       (fn [chunk]
         (let [chunk-tag (identify-chunk-type chunk sorted-line-end-pairs)]
           (parse-chunk
             chunk
             token-map
             {:tag chunk-tag :attrs {} :content []})))

       (chunk-group-tokenize text (keys line-starts-elems))))))
