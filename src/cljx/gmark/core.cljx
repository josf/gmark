(ns gmark.core
  (:require
   [gmark.tei-elems :as t]
   [gmark.parse :as p]))


(defn to-gmark [elem tagtypes]
  (let [etype ((:tag elem) tagtypes)]
    (when (t/can-contain-text? etype)
      (t/elem-to-text elem tagtypes))))

(defn parse-gmark-text [text token-map root]
  (p/parse-chunk text token-map root))

(defn parse-gmark-multi-text [text tagtypes root]
  (let [token-map (t/tagtypes-to-token-map tagtypes)
        starts-types (t/chunk-line-starts-types tagtypes)]
   (p/parse-chunk-group
     root
     text
     starts-types
     token-map)))
