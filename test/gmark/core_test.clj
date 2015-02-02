(ns gmark.core-test
  (:require [clojure.test :refer :all]
            [gmark.core :refer :all]))


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
  (let [mrk (to-gmark simple-fragment)]
    (is (= mrk "[n=1] stuff I like to say\n[n=2]I say it every day"))))
