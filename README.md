protoclj
========

A protobuf compiler for Clojure wrappers

Goals
_____
* All reflection done at compile time, so it's really fast
* Play well with IOFactory, so that protobufs can be cast to and from InputStreams
* Handle Nested Objects, Repeated Objects and Enums
* Access fields by keywords

Usage
_____
* Add to project deps somehow

```clojure
; Declare the functions key-value-pair and nested-object
; Sample1 is the root of the proto definition.
(defprotos sample1 Sample1
  key-value-pair  Sample1$KeyValuePair
  nested-object   Sample1$NestedObject)
  
(def object (key-value-pair {:key "foo" :value "bar"}))

(def back-to-map (mapify object)) ; -> {:key "foo" :value "bar"}

(clojure.java.io/input-stream object) ; -> BufferedInputStream (for ring)
```
