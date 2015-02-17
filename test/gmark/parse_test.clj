(ns gmark.parse_test
  (:require [clojure.test :refer :all]
            [gmark.parse :refer :all]))

(deftest tokenize-simple
  (is (=
        (tokenize "the //stuff// we like")
        ["the " {:token "//" :type :either} "stuff" {:token "//" :type :either} " we like"])))
