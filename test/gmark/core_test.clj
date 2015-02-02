(ns gmark.core-test
  (:require [clojure.test :refer :all]
            [gmark.core :refer :all]
            [gmark.tei-elems :as t]))


(def simple-fragment
  {:tag :lg
   :attrs {:type "stanza"}
   :content [{:tag :l
              :attrs {:n 1}
              :content ["stuff I like to say"]}
             {:tag :l
              :attrs {:n 2}
              :content ["I say it every day"]}]})


(deftest test-transform-lg
  (let [mrk (to-gmark simple-fragment t/tei)]
    (is (= mrk "\n\n-stuff I like to say\n-I say it every day\n\n"))))
