(ns protoclj.sample.protos
  (:require [fixtures.sample :refer :all])
  (:import
   java.io.ByteArrayInputStream
   java.io.InputStream
   protoclj.sample.protos.Sample1$KeyValuePair))

;;; Helpers

(defmacro set-optional [obj attr-sym value]
  `(let [v# ~value]
     (if (nil? v#)
       ~obj
       (~(symbol (str ".set" attr-sym)) ~obj v#))))

(defmacro get-optional [obj attr-sym]
  `(let [o# ~obj]
     (if (~(symbol (str ".has" attr-sym)) o#)
       (~(symbol (str ".get" attr-sym)) o#)
       nil)))

(defmacro value-of
  "A way to get no reflection warnings but have the code look clean"
  [enum-class s]
  `(~(symbol (str enum-class "/valueOf"))
    ~(vary-meta s assoc :tag 'String)))

(defmacro defbuilder
  "Applies each line of body to a new builder for the supplied class-sym.
   Handles type-hinting of it's return value for you."
  [name class-sym args & body]
  `(defn ~(vary-meta name assoc :tag class-sym)
     ~args
     (-> (~(symbol (str class-sym "/newBuilder")))
         ~@body
         .build)))

;;; Shared

(defn KeyValuePair->vec [^Sample1$KeyValuePair kvp]
  [(.getKey kvp) (.getValue kvp)])

(defbuilder ->KeyValuePair Sample1$KeyValuePair
  [[k v]]
  (.setKey k)
  (.setValue v))
