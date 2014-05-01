(ns protoclj.protoj-test
  (:require [protoclj.core :refer :all]
            [clojure.test :refer :all])
  (:import [protoclj
            Sample1
            Sample1$KeyValuePair
            Sample1$NestedObject
            Sample1$RepeatedObject
            Sample1$OptionalObject
            Sample1$EmbeddedObject
            Sample1$EmbeddedObject$AnonymousObject
            Sample1$EnumObject
            Sample1$EnumObject$TheEnum]))

(set! *warn-on-reflection* true)

(defprotos sample1 Sample1
  key-value-pair  Sample1$KeyValuePair
  nested-object   Sample1$NestedObject
  repeated-object Sample1$RepeatedObject
  optional-object Sample1$OptionalObject
  embedded-object Sample1$EmbeddedObject
  enum-object     Sample1$EnumObject)

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
        (is (= {:key "foo" :value "bar"} (mapify kvp))))))

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
        (is (= {:name "name" :kvp {:key "foo" :value "bar"}} (mapify nested))))))

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
        (is (= {:messages ["foo"] :kvps [{:key "foo" :value "bar"}]} (mapify proto))))))

  (testing "it can be parsed from a map"
    (let [proto (repeated-object {:messages ["foo" "bar"] :kvps [{:key "foo" :value "bar"}]})]
      (is (= ["foo" "bar"] (proto-get proto :messages)))
      (is (= "foo" (-> proto (proto-get :kvps) first (proto-get :key)))))))

(deftest a-protobuf-with-an-optional-object
  (testing "nils out the field"
    (let [proto-object (-> (Sample1$OptionalObject/newBuilder)
                           .build)
          proto (optional-object proto-object)]
      (is (= nil (proto-get proto :text)))

      (testing "can be turned into a map"
        (is (= {:text nil} (mapify proto)))))))

(deftest a-protobuf-with-an-embedded-message
  (testing "can be read from"
    (let [proto-object (-> (Sample1$EmbeddedObject/newBuilder)
                           (.setObj (-> (Sample1$EmbeddedObject$AnonymousObject/newBuilder)
                                        (.setText "foo")
                                        .build))
                           .build)
          proto (embedded-object proto-object)]
      (is (= "foo" (-> proto (proto-get :obj) (proto-get :text))))

      (testing "can be turned into a map"
        (is (= {:obj {:text "foo"}} (mapify proto))))))

  (testing "it can be parsed from a map"
    (let [proto (embedded-object {:obj {:text "foo"}})]
      (is (= "foo" (-> proto (proto-get :obj) (proto-get :text)))))))

(deftest a-protobuf-with-an-enum
  (testing "can be read from"
    (let [proto-object (-> (Sample1$EnumObject/newBuilder)
                           (.setTheEnum Sample1$EnumObject$TheEnum/FIRST)
                           .build)
          proto (enum-object proto-object)]
      (is (= :FIRST (-> proto (proto-get :the-enum) proto-val)))

      (testing "can be turned into a map"
        (is (= {:the-enum :FIRST} (mapify proto))))))

  (testing "it can be parsed from a map"
    (let [proto (enum-object {:the-enum :FIRST})]
      (is (= :FIRST (-> proto (proto-get :the-enum) proto-val))))))

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
