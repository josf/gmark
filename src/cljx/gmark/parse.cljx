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
         nil
         (:closing-tag info))])
    token-map))


(defn match-by-length [string [token-begin token-end] possible-ends]
  {:pre [(list? possible-ends)]}
  "Matches either token-begin or token-end against string. Matching is
  length based in that if a token is n chars long, we just match
  against the first n chars in string. End tokens match only if they
  occur at the end of the string. Empty elements are represented a nil
  as their end token."
  (cond
    (and
      (>= (count string) (count token-begin))
      (= token-begin (subs string 0 (count token-begin))))
    {:token token-begin
     :type (if (= token-begin (first possible-ends))
             :end
             (if (nil? token-end)
               :empty
               :begin))}

    (and
      (>= (count (str/trim string)) (count token-end))
      (= token-end (subs string 0 (count token-end)))
      (= token-end (first possible-ends)))
    {:token token-end :type :end}))

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
  (let [tokes (sort-token-groups (conj tokens ["#[" "]"]))
        begins-to-ends (into {} tokes)]
    (loop [accum []
           s string
           possible-ends '()]          ; what would be a valid end tag
      (if (empty? s)
        accum
        (if-let [match (first (keep
                                #(match-by-length s % possible-ends)
                                tokes))]
          (recur
            (conj accum match)
            (subs s (count (:token match)))
            (if (= :begin (:type match))
              ;; push corresponding end tag onto possible-ends
              (conj possible-ends (get begins-to-ends (:token match)))
              ;; pop matched end tag off of possible-ends 
              (rest possible-ends)))
          
          (recur
            (char-to-accum (first s) accum)
            (subs s 1)
            possible-ends))))))


(defn att-str-to-map [att-str]
  "Takes a string of attribute names and val pairs, in the form
  name1:val1::name2:val2 etc. and returns a map names to values. When
  no name is supplied, ie. val1::name2:val2 the value is assigned to
  the gmark-default attribute."
  (into {}
    (map
      #(let [splat  (str/split % #":")]
         (if (= 2 (count splat))
           [(keyword (first splat)) (second splat)]
           [:gmark-default (first splat)]))
      (str/split att-str #"::"))))


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
      (let [next-is-attrs? (= "#[" (:token (fnext tokenized)))]
        (structure
          (if next-is-attrs?
            (nnext (nnext tokenized))    ; skip 1) #[ 2) blah:blah 3) ]
            (next tokenized))
         (assoc root
           :content (conj
                      (:content root)
                      {:tag  (:tag (get token-map (:token token)))
                       :attrs (if next-is-attrs?
                                (att-str-to-map (first (nnext tokenized)))
                                {})
                       :content []}))
         token-map))

      (and (map? token) (contains? token :token) (not (:no-content token)))
      (let [next-is-attrs? (and
                             (map? (fnext tokenized))
                             (= "#[" (:token (fnext tokenized))))
            elem-type (get token-map  (:token token))
            closing-token (:closing-tag elem-type)]

        (structure
          (drop-with-first ; avoid the tokens that go to the new element
            #(and (map? %) (= closing-token (:token %)))
            (if next-is-attrs?
              (nnext (nnext tokenized))  ; skip to 4th elem if attrs...
              (next tokenized)))        ;  ...or just keep going
          
          (assoc root                  ; same old root element but...
            :content
            (conj                      ;... we append...
              (:content root)
              (structure           ;... the new element that receives
                (take-while ; the tokens that precede the next closing tag
                  #(not
                     (and (map? %) (= closing-token (:token %))))
                  (if next-is-attrs?
                    (nnext (nnext tokenized))
                    (next tokenized)))
                {:tag (:tag elem-type)
                 :attrs (if next-is-attrs?
                          (att-str-to-map (first (nnext tokenized)))
                          {})
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
  (str/replace reg-str #"([.\*?])" "\\\\$1"))

(defn chunking-regex [line-starts]
  (re-pattern
    (apply str
      (interpose
        "|"
        (conj
          (map #(str "(?=\\n" (pre-clean-regex %) "\\s+)") line-starts)
          "\n\n")))))


(defn chunk-remove-line-start [chunk line-start]
  "Trim leading line start from a chunk string. If not found, just
  returns the original string. (This might be important for paragraph
  elements that don't have a line-start string.)"
  (let [ls-len (inc (count line-start))] ; inc b/c of extra space
    (if (= (str line-start " ") (subs chunk 0 ls-len))
      (subs chunk ls-len)
      chunk)))


(defn chunk-group-tokenize [text line-starts]
  "line-starts is a list of possible line beginning marker
  strings. With an empty list, defaults to '\n\n', meaning simple
  paragraphs."
  (map str/trim
   (if (empty? line-starts)
     (str/split text #"\n\n")
     (str/split text (chunking-regex line-starts)))))


(defn identify-chunk-type [chunk sorted-line-end-pairs]
  "Returns a vector containing the line beginning string and the
  corresponding element type."
  (first
   (filter
     (fn [[line-end elem-kw]]
       (and
         (> (count chunk) (count line-end))
         (=
           (str line-end " ")
           (subs chunk 0 (inc (count line-end))))))
     sorted-line-end-pairs)))

(defn check-for-chunk-level-attributes [chunk]
  "To be called on a chunk that no longer has its line end
  marker. Returns an attribute map (possibly empty) and the initial
  text, minus the attr-string if it was initially present. "
  (if-let [mtch (re-find #"^\s*#\[([^]]+)\]\s*(.*)" chunk)]
    [(att-str-to-map (get mtch 1))  (get mtch 2)]
    [{} chunk]))




(defn parse-chunk-group [parent text line-starts-elems token-map]
  "line-starts-elems is a map of line-start strings to element type
  keywords."
  (let [sorted-line-end-pairs (sort-token-groups line-starts-elems)]
   (assoc parent
     :content
     (mapv

       (fn [chunk]
         (let [[line-end chunk-tag]
               (identify-chunk-type chunk sorted-line-end-pairs)]
           (when (nil? chunk-tag)
             (throw (#+clj IllegalStateException.
                      #+cljs js/Error.
                      (str "null chunk-tag: " (str
                                                (apply str sorted-line-end-pairs)
                                                " "
                                                chunk)))))
           (let [[attrs final-line] (check-for-chunk-level-attributes
                                      (chunk-remove-line-start chunk line-end))]
            (parse-chunk
              final-line
              token-map
              {:tag chunk-tag :attrs attrs :content []}))))

       (chunk-group-tokenize text (keys line-starts-elems))))))
