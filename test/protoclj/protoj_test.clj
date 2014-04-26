(ns protoclj.protoj-test
  (:require [protoclj.protoj :refer :all]
            [clojure.test :refer :all])
  (:import [protoclj Sample1$KeyValuePair]))

(defprotos sample1
  key-value-pair Sample1$KeyValuePair)

(deftest simple-key-can-be-read-and-written

  (testing "it can read key and value"
    (let [proto-object (-> (Sample1$KeyValuePair/newBuilder)
                           (.setKey "foo")
                           (.setValue "bar")
                           .build)
          kvp (key-value-pair proto-object)]
      (is (= "foo" (proto-get kvp :key)))
      (is (= "bar" (proto-get kvp :value))))))
