(ns gmark.parse-test
  (:require [clojure.test :refer :all]
            [gmark.parse :refer :all]))

(deftest tokenize-simple
  (is (=
        (tokenize "the //stuff// we like"  [["//" "//"]])
        ["the " {:token "//" :type :either} "stuff" {:token "//" :type :either} " we like"])
    (=
      (tokenize "the //stuff// we {{like}}" [["//" "//"] ["{{" "}}"]])
      ["the " {:token "//" :type :either} "stuff"
       {:token "//" :type :either} " we " {:token "{{" :type :begin} "like"
       {:token "}}" :type :end}])))


(deftest match-by-length-test
  (is (map? (match-by-length "//blah" ["//" "//"]))))

(deftest match-by-length-test-neg
  (is (nil? (match-by-length "blah//" ["//" "//"]))))

(deftest structure-simple
  (let [token-map  {"//"
                    {:tag :em :closing-tag "//"}
                    "|"
                    {:tag :caesura :no-content true}}
        tokenized ["stuff" {:token "//" :type :either} "we"
                   {:token "//" :type :either} "like "
                   {:token "|" :type :either} " do"]
        structured (structure
                     tokenized
                     {:tag :root :attrs {} :content []}
                     token-map)]
    (is (map? structured))
    (is (= (:tag structured) :root))
    (is (= "stuff" (first (:content structured))))
    (is (= {:tag :em :attrs {} :content ["we"]} (second (:content structured))))
    (is (= {:tag :caesura :attrs {} :content []}
          (nth (:content structured) 3)))
    (is (= " do" (last (:content structured))))))
