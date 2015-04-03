(ns gmark.parse-test
  (:require [clojure.test :refer :all]
            [gmark.parse :refer :all]))


(deftest tokens-from-token-map-basic
  (let [token-map  {"//" {:tag :em
                          :closing-tag "//"}
                    "{{" {:tag :strong
                          :closing-tag "}}"}}
         result (tokens-from-token-map token-map)] 
    (is (some #(= % ["//" "//"]) result))
    (is (some #(= % ["{{" "}}"]) result))))

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
  (is (map? (match-by-length "//blah" ["//" "//"])))
  (is (map? (match-by-length "//blah " ["//" "//"])))
  (is (map? (match-by-length "{{blah}}" ["{{" "}}"])))
  (is (map? (match-by-length "{{blah}} " ["{{" "}}"]))))

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


(deftest regex-cleaning
  (is (= "\\*\\*" (pre-clean-regex "**")))
  (is (= "\\." (pre-clean-regex ".")))
  (is (= "\\?" (pre-clean-regex "?"))))

(deftest chunking-regex-test
  (let [txt "- line one\n- line two\n- line three"
        cregex (chunking-regex ["-"])
        chunked (re-find cregex txt)]
    (is (not (nil? chunked)))))

(deftest chunk-group-tokenize-simple-test
  (let [txt  "- line one\n- line two\n- line three"
        chunked (chunk-group-tokenize txt ["-"])]
    (is (= 3 (count chunked)))
    (is (= (first chunked) "- line one"))
    (is (= (nth chunked 2) "- line three"))))

(deftest chunk-group-tokenize-inner-line-break-test
  (let [
        ;; inner line break: no effect
        txt  "- line one\n- line\n two\n- line three"

        ;; double inner line break: paragraph logic kicks in
        txt2 "- line one\n- line\n\n two\n- line three"

        ;; double line breaks between groups: is this desirable?
        ;; in multi-chunk context
        txt3 "- line one\n\n- line two\n- line three"
        ;; leading newlines
        txt4 "\n- line one\n- line two\n- line three"
        
        chunked (chunk-group-tokenize txt ["-"])
        chunked2 (chunk-group-tokenize txt2 ["-"])
        chunked3 (chunk-group-tokenize txt3 ["-"])
        chunked4 (chunk-group-tokenize txt4 ["-"])]
    (is (= 3 (count chunked)))
    (is (= (first chunked) "- line one"))
    (is (= (nth chunked 2) "- line three"))
    (is (= 4 (count chunked2)))
    (is (= 3 (count chunked3)))
    (is (= (second chunked3) "- line two"))
    (is (= 3 (count chunked4)) "chunk-group-tokenize w leading spaces")))

(deftest identify-chunk-type-test
  (is (= ["-" :l] (identify-chunk-type "- bloop" [["-" :l]])))
  (is (= ["-" :l] (identify-chunk-type "- bloop" [["**" :head] ["-" :l]])))
  (is (= ["**" :head] (identify-chunk-type "** zork" [["**" :head] ["-" :l]])))
  (is (nil? (identify-chunk-type "~ bloop" [["**" :head] ["-" :l]]) )))

(deftest parse-chunk-group-simple-test
  (let [parent {:tag :bogus :attrs {} :content []}
        text "- line //one//\n- line two\n- line three"
        line-starts-elems {"-" :l}
        token-map {"//" {:tag :rhyme :closing-tag "//"}}
        parsed (parse-chunk-group parent text line-starts-elems token-map)]
    (is (map? parsed))
    (is (= :l (:tag (first (:content parsed)))))
    (is (= :rhyme (:tag (second (:content (first (:content parsed)))))))))

; "\*\*" :head
