(ns protoclj.protoj-test
  (:require [protoclj.protoj :refer :all]
            [clojure.test :refer :all])
  (:import [protoclj Sample1$KeyValuePair
                     Sample1$NestedObject
                     Sample1$RepeatedObject]))

(set! *warn-on-reflection* true)

(defprotos sample1
  key-value-pair  Sample1$KeyValuePair
  nested-object   Sample1$NestedObject
  repeated-object Sample1$RepeatedObject)

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
        (is (= {:key "foo" :value "bar"} (->map sample1 kvp))))))

  (testing "can be parsed from a map"
    (let [kvp (key-value-pair {:key "foo" :value "bar"})]
      (is (= "foo" (proto-get kvp :key)))
      (is (= "bar" (proto-get kvp :value))))))

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
        (is (= {:name "name" :kvp {:key "foo" :value "bar"}} (->map sample1 nested))))))

  (testing "can be parsed from a map"
    (let [nested (nested-object {:name "name" :kvp {:key "foo" :value "bar"}})]
      (is (= "name" (proto-get nested :name)))
      (is (= "foo" (proto-get (proto-get nested :kvp) :key)))
      (is (= "bar" (proto-get (proto-get nested :kvp) :value)))))

  (testing "throws an exception with data missing"
    (is (thrown? com.google.protobuf.UninitializedMessageException
                 (nested-object {:name "name"})))))

(deftest a-protobuf-with-a-repeated-field
  (testing "can be read from"
    (let [proto-object (-> (Sample1$RepeatedObject/newBuilder)
                           (.addMessages "foo")
                           (.addKvps (-> (Sample1$KeyValuePair/newBuilder)
                                         (.setKey "foo")
                                         (.setValue "bar")
                                         .build))
                           .build)
          proto (repeated-object proto-object)]
      (is (= ["foo"] (proto-get proto :messages)))
      (is (= "foo" (-> proto (proto-get :kvps) first (proto-get :key))))

      (testing "can be turned into a map"
        (is (= {:messages ["foo"] :kvps [{:key "foo" :value "bar"}]} (->map sample1 proto))))))

  (testing "it can be parsed from a map"
    (let [proto (repeated-object {:messages ["foo" "bar"] :kvps [{:key "foo" :value "bar"}]})]
      (is (= ["foo" "bar"] (proto-get proto :messages)))
      (is (= "foo" (-> proto (proto-get :kvps) first (proto-get :key)))))))

(deftest coersing-to-a-protobuf
  (let [proto-object (-> (Sample1$KeyValuePair/newBuilder)
                        (.setKey "foo")
                        (.setValue "bar")
                        .build)]
    (testing "it reads from a byte array"
      (let [byte-array (.toByteArray proto-object)
            kvp (key-value-pair byte-array)]

        (is (= "foo" (proto-get kvp :key)))
        (is (= "bar" (proto-get kvp :value)))))

    (testing "it reads from an input stream"
      (let [input-stream (-> proto-object .toByteArray clojure.java.io/input-stream)
            kvp (key-value-pair input-stream)]
        (is (= "foo" (proto-get kvp :key)))
        (is (= "bar" (proto-get kvp :value)))))

    (testing "it can be turned into an input stream"
      (let [input-stream (-> proto-object key-value-pair clojure.java.io/input-stream)]
        (is (not (nil? input-stream)))))))
