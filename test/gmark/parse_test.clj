(ns gmark.parse_test
  (:require [clojure.test :refer :all]
            [gmark.parse :refer :all]))

(deftest test-combined-regex
  (is (= "(//)|(\\*)" (combined-regex-pattern ["//" "*"])))
  (is (= "(//)|(\\*)" (combined-regex-pattern ["//" "*" "*" "//"]))))

(deftest test-combined-regex-escaping
  (is (= "(//)|(\\*)|(\\{)|(\\})")
    (combined-regex-pattern ["//" "*" "{" "}"])))
