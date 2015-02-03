(ns gmark.parse
  (:require [clojure.string :as str]))


(defn combined-regex-pattern
  "returns the string. you still need to call re-pattern."
  [tokens]
  (let [escape-chars {\* "\\*" \{ \}}]
   (->> tokens
     distinct
     (map #(str "(" (str/escape % escape-chars) ")"))
     (interpose "|")
     (apply str))))

(defn tokenize-text
  "Divide a string into its tokens. token-pairs is a sequence of one
  or two item vectors."
  [txt token-pairs]
  {:pre [(string? txt) (seq? token-pairs)]}
  ()
  )
