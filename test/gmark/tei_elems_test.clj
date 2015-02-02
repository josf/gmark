(ns gmark.tei-elems-test
  (:require [clojure.test :refer :all]
            [gmark.tei-elems :refer :all]))


(deftest test-basic-protocol-setup
  "clear our throat and check our protocols"
  (let [chunk (chunk-type [:lg] "-")]
    (is (= true (text-edit-as-child? chunk)))
    (is (= true (can-contain-text? chunk)))))


(deftest test-chunk-to-text
  "output gmarkup from l (verse line) element"
  (let [base-elem {:tag :l
                   :attrs {}
                   :content []}
        all-text-line (elem-to-text
                        (assoc base-elem :content ["My verse"])
                        tei)
        nested-cesure (elem-to-text
                        (assoc base-elem
                          :content ["My "
                                    {:tag :caesura :attrs {} :content []}
                                    "cesure"])
                        tei)
        nested-note (elem-to-text
                      (assoc base-elem
                        :content ["My "
                                  {:tag :note
                                   :attrs {}
                                   :content ["annotated"]}
                                  " verse"])
                      tei)]
    (is (= all-text-line "\n-My verse\n"))
    (is (= nested-cesure "\n-My |cesure\n"))
    (is (= nested-note "\n-My {annotated} verse\n"))))


(deftest test-multi-chunk-to-text
  (let [mc {:tag :lg
            :attrs {}
            :content [
                      {:tag :l
                       :attrs {}
                       :content ["My verse"]}]}]
    (is (= (elem-to-text mc tei)
          "\n\n-My verse\n\n"))))



