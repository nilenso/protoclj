(ns protoclj.protoj-test
  (:require [protoclj.protoj :refer :all]
            [clojure.test :refer :all])
  (:import [protoclj Sample1$KeyValuePair]))

(defprotos sample1
  key-value-pair Sample1$KeyValuePair)

(deftest very-simple-protobufs

  (testing "can be read from"
    (let [proto-object (-> (Sample1$KeyValuePair/newBuilder)
                           (.setKey "foo")
                           (.setValue "bar")
                           .build)
          kvp (key-value-pair proto-object)]
      (is (= "foo" (proto-get kvp :key)))
      (is (= "bar" (proto-get kvp :value)))
      (is (= proto-object (proto-obj kvp)))

      (testing "can be turned into a map"
        (is (= {:key "foo" :value "bar"} (->map sample1 kvp)))))))
