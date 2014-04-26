(ns protoclj.protoj-test
  (:require [protoclj.protoj :refer :all]
            [clojure.test :refer :all])
  (:import [protoclj Sample1$KeyValuePair
                     Sample1$NestedObject]))

(set! *warn-on-reflection* true)

(defprotos sample1
  key-value-pair Sample1$KeyValuePair
  nested-object  Sample1$NestedObject)

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

(deftest a-protobuf-containing-another
  (testing "can be read from"
    (let [proto-object (-> (Sample1$NestedObject/newBuilder)
                           (.setName "name")
                           (.setKvp (-> (Sample1$KeyValuePair/newBuilder)
                                        (.setKey "foo")
                                        (.setValue "bar")
                                        .build))
                           .build)
          nested (nested-object proto-object)]
      (is (= "name" (proto-get nested :name)))
      (is (= "foo" (proto-get (proto-get nested :kvp) :key)))
      (is (= "bar" (proto-get (proto-get nested :kvp) :value)))

      (testing "can be turned into a map"
        (is (= {:name "name" :kvp {:key "foo" :value "bar"}} (->map sample1 nested)))))))

(deftest coersing-to-a-protobuf
  (let [proto-object (-> (Sample1$KeyValuePair/newBuilder)
                        (.setKey "foo")
                        (.setValue "bar")
                        .build)]
    (testing "it reads from a byte array"
      (let [byte-array (.toByteArray proto-object)
            kvp (key-value-pair byte-array)]

        (is (= "foo" (proto-get kvp :key)))
        (is (= "bar" (proto-get kvp :value)))))))
