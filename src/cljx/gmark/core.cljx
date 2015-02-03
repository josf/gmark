(ns gmark.core
  (:require [gmark.tei-elems :as t]))


(defn to-gmark [elem tagtypes]
  (let [etype ((:tag elem) tagtypes)]
    (when (t/can-contain-text? etype)
      (t/elem-to-text elem tagtypes))))
