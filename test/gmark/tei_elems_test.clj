(ns gmark.tei-elems-test
  (:require [clojure.test :refer :all]
            [gmark.tei-elems :refer :all]))


(deftest test-basic-protocol-setup
  "clear our throat and check our protocols"
  (let [chunk (chunk-type [:lg] "-")]
    (is (= true (text-edit-as-child? chunk)))
    (is (= true (can-contain-text? chunk)))))


(deftest empty-elements
  (let [tt {:caesura {:type :empty
                      :begin-token "|"
                      :end-token "|"}}]
    (is (= "|" (inner-to-text {:tag :caesura :attrs {} :content []} tt)))))

(deftest inner-elements
  (let [tt {:caesura {:type :empty
                      :begin-token "|"
                      :end-token "|"}
            :rhyme {:type :inner
                    :begin-token "//"
                    :end-token "//"}}
        txt-w-cesure  (apply str
                        (map #(inner-to-text % tt)
                          ["my "
                           {:tag :caesura :attrs {} :content []}
                           "verse"]))
        nested-note (inner-to-text
                      {:tag :lg
                       :attrs {}
                       :content ["My "
                                 {:tag :note
                                  :attrs {}
                                  :content ["annotated"]}
                                 " verse"]}
                      {:note {:type :inner
                              :begin-token "{"
                              :end-token "}"}})]
    (is (string? txt-w-cesure) "if this is not a string, big trouble")
    (is (= txt-w-cesure "my |verse") "text around empty element")
    (is (= nested-note "My {annotated} verse"))))

(deftest test-chunk-to-text
  "output gmarkup from l (verse line) element"
  (let [base-elem {:tag :l
                   :attrs {}
                   :content ["My "
                             {:tag :caesura :attrs {} :content []}
                             "cesure"]}
        tt  {:l {:type :chunk
                             :line-token "-"}
                         :lg {:type :multi-chunk}
                         :caesura {:type :empty
                                   :begin-token "|"
                                   :end-token "|"}}
        nested-cesure (chunk-to-text base-elem tt true)
        nested-cesure-2 (chunk-to-text base-elem tt)]
    (is (= nested-cesure "\n- My |cesure")
      "Insert empty sub tag (from multi-chunk)")
    (is (= nested-cesure-2 "My |cesure")
      "Insert empty sub tag (not from multi-chunk)")))


(deftest test-multi-chunk-to-text
  (let [mc {:tag :lg
            :attrs {}
            :content
            [{:tag :l
              :attrs {}
              :content ["My verse"]}]}
        parsed  (multi-chunk-to-text mc
                  {:lg {:type :multi-chunk}
                   :l {:type :chunk
                       :line-token "-"}})]
    (is (= parsed "\n\n- My verse\n") "Simple multi-chunk-to-text")))


(deftest test-description-to-tagtypes
  (let [tt (tagtypes {:body {:type :container
                             :contains [:div]}
                      :rhyme {:type :inner
                              :contains []
                              :begin-token "\\"
                              :end-token "\\"}})]
    (is (map? tt))
    (is (contains? tt :body))
    (is (contains? tt :rhyme))))


(deftest elem-to-text-test
  (let [tt {:lg {:type :multi-chunk}
            :l {:type :chunk
                :line-token "-"}
            :caesura {:type :empty
                      :begin-token "|"
                      :end-token "|"}
            :note {:type :inner
                   :begin-token "{"
                   :end-token "}"}
            :rhyme {:type :inner
                    :begin-token "//"
                    :end-token "//"}}]
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
