(ns gmark.tei-elems-test
  (:require [clojure.test :refer :all]
            [gmark.tei-elems :refer :all])
  (:import [gmark.tei_elems ChunkEType MultiChunkEType ContainerEType InnerEType EmptyEType]))


(deftest test-basic-protocol-setup
  "clear our throat and check our protocols"
  (let [chunk (chunk-type [:lg] "-")]
    (is (= true (text-edit-as-child? chunk)))
    (is (= true (can-contain-text? chunk)))))


(deftest empty-elements
  (let [tt  {:caesura (empty-type "|")}]
    (is (= "|" (elem-to-text {:tag :caesura :attrs {} :content []} tt)))))

(deftest inner-elements
  (let [tt {:caesura (empty-type "|")
            :rhyme (inner-type "//" "//")
            :note (inner-type "{" "}")
            :lg (multi-chunk-type [])}
        txt-w-cesure  (apply str
                        (map #(elem-to-text % tt)
                          ["my "
                           {:tag :caesura :attrs {} :content []}
                           "verse"]))]
    (is (string? txt-w-cesure) "if this is not a string, big trouble")
    (is (= txt-w-cesure "my |verse") "text around empty element")))

(deftest test-chunk-to-text
  "output gmarkup from l (verse line) element"
  (let [base-elem {:tag :l
                   :attrs {}
                   :content ["My "
                             {:tag :caesura :attrs {} :content []}
                             "cesure"]}
        tt  {:l (chunk-type [:caesura] "-")
             :lg (multi-chunk-type [:l])
             :caesura (empty-type "|")}
        nested-cesure (elem-to-text base-elem tt)
        nested-cesure-2 (elem-to-text base-elem tt)]
    (is (= nested-cesure-2 "My |cesure")
      "Insert empty sub tag (not from multi-chunk)")))


(deftest test-multi-chunk-to-text
  (let [mc {:tag :lg
            :attrs {}
            :content
            [{:tag :l
              :attrs {}
              :content ["My verse"]}
             {:tag :l
              :attrs {}
              :content ["Another line"]}
             "random string"]}
        nested-note {:tag :lg
                     :attrs {}
                     :content ["My "
                               {:tag :note
                                :attrs {}
                                :content ["annotated"]}
                               " verse"]}
        tt {:lg (multi-chunk-type [:l])
            :l (chunk-type [:caesura] "-")
            :note (inner-type "{" "}")}
        parsed  (elem-to-text mc tt)]
    (is (= parsed "\n\n- My verse\n- Another line\n\n")
      "Simple multi-chunk-to-text")))
;;;     (is (= nested-note "My {annotated} verse"))


(deftest test-description-to-tagtypes
  (let [tt {:body (container-type [:div])
            :rhyme (inner-type "//" "//")}]
    (is (map? tt))
    (is (contains? tt :body))
    (is (contains? tt :rhyme))))


(deftest elem-to-text-test
  (let [tt
        {:l (chunk-type [:caesura] "-")
         :lg (multi-chunk-type [:l])
         :caesura (empty-type "|")
         :note (inner-type "{" "}")
         :rhyme (inner-type "//" "//")}]
    (is (= "strang" (elem-to-text "strang" tt)) "text is just text")
    (is (= "my verse" (elem-to-text {:tag :l :attrs {} :content ["my verse"]} tt))
      "Chunk with just text content")
    (is (= "\n- my verse\n- more verse")
      (elem-to-text
        {:tag :lg
         :attrs {}
         :content
         [{:tag :l
           :attrs {}
           :content ["my verse"]}
          {:tag :l
           :attrs {}
           :content ["more verse"]}]}
        tt))))


(deftest chunk-line-starts-types-test
  (let [st (chunk-line-starts-types
             {:l (chunk-type  [:note] "-")
              :lg (multi-chunk-type [:l])})]
    (is (map? st))
    (is (= 1 (count st)) "Should be exactly one type left")
    (is (= :l (get st "-")))))


(deftest tagtyptes-to-token-map-test
  (let [tt {:l (chunk-type [:caesura] "-")
            :lg (multi-chunk-type [:l])
            :caesura (empty-type "|")
            :note (inner-type "{" "}")
            :rhyme (inner-type "//" "//")}
        tm (tagtypes-to-token-map tt)]
    (is (map? tm))
    (is (= :caesura (:tag (get tm "|"))) "values are maps (empty type)")
    (is (= :rhyme (:tag (get tm "//"))) "values are maps (inner type)")
    (is (= :note (:tag (get tm "{"))) "tag values are present")))


(deftest attribute-output-test
  (is (= "" (attributes-to-text {})) "empty attrs = empty string")
  (is (= "#[attr:value]" (attributes-to-text {:attr "value"}))))

(deftest attribute-default-on-etypes-test
  (let [ct-w-default (chunk-type [:rhyme] "-" {:attribute-default :n})
        ct-wo-default (chunk-type [:rhyme] "*")]
    (is (= :n (attribute-default ct-w-default)))
    (is (thrown? IllegalStateException (attribute-default ct-wo-default)))))


(deftest elem-to-text-with-attributes
  (let [tt  {:l (chunk-type [:caesura] "-" {:attribute-default :n})
             :lg (multi-chunk-type [:l] {:attribute-default :type})
             :caesura (empty-type "|")
             :note (inner-type "{" "}")
             :rhyme (inner-type "//" "//")}
        l {:tag :l :attrs {:n 1} :content ["yo"]}
        lg {:tag :lg :attrs {:type :stanza}
             :content
             [{:tag :l :attrs {:n 1} :content ["yo"]}
              {:tag :l :attrs {:n 2} :content ["yo yo"]}]}]
    (is (= "#[1] yo" (elem-to-text l tt)))
    ))
