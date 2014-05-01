(ns protoclj.core
  (:require [clojure.java.io]
            [protoclj.reflection :as reflection]
            [protoclj.sexp :as sexp]))

(defprotocol ProtobufElement
  (proto-obj [m])
  (mapify [m]))

(defprotocol ProtobufMap
  (proto-get [m k])
  (proto-get-raw [m k])
  (proto-keys [m]))

(extend-protocol ProtobufElement
  nil
  (proto-obj [m] nil)
  (mapify [m] nil))

(extend-protocol ProtobufMap
  nil
  (proto-get [m k] nil)
  (proto-get-raw [m k] nil)
  (proto-keys [m] nil))

(defn- build-reader
  "The main reify that can get different params"
  [this attributes bindings-map]
  `(reify
     ProtobufMap
     (proto-get [_# k#]
       (case k#
         ~@(mapcat #(list (:name-kw %) (sexp/fetch-as-object this bindings-map %)) attributes)
         nil))
     (proto-get-raw [_# k#]
       (case k#
         ~@(mapcat #(list (:name-kw %) (sexp/fetch-as-object this nil %)) attributes)
         nil))
     (proto-keys [_#] ~(vec (map :name-kw attributes)))

     ProtobufElement
     (proto-obj [_#] ~this)
     (mapify [a#]
       (hash-map
        ~@(mapcat #(list (:name-kw %) (sexp/fetch-as-primitive this bindings-map %)) attributes)))

     clojure.java.io.IOFactory
     (make-input-stream [x# opts#]
       (clojure.java.io/make-input-stream (.toByteArray ~this) opts#))
     (make-output-stream [x# opts#]
       (throw (IllegalArgumentException. (str "Cannot create an output stream from protobuf"))))
     (make-reader [x# opts#]
       (clojure.java.io/make-reader (clojure.java.io/make-input-stream x# opts#) opts#))
     (make-writer [x# opts#]
       (clojure.java.io/make-writer (clojure.java.io/make-output-stream x# opts#) opts#))))

(defn- build-proto-definition
  "Returns a sexp for defining the proto"
  [clazz fn-name protocol-name bindings-map]
  (let [this (vary-meta (gensym "this") assoc :tag clazz)
        attributes (reflection/proto-attributes clazz)
        byte-array-type (-> 0 byte-array class .getName symbol)
        byte-array-symbol (vary-meta (gensym "stream") assoc :tag byte-array-type)
        map-sym (gensym "map")]
    `(extend-protocol ~protocol-name
       nil
       (~fn-name [_#] nil)

       ~byte-array-type
       (~fn-name [~byte-array-symbol] (~fn-name (. ~clazz ~'parseFrom ~byte-array-symbol)))

       java.io.InputStream
       (~fn-name [input-stream#] (~fn-name (. ~clazz ~'parseFrom input-stream#)))

       ~clazz
       (~fn-name [~this] ~(build-reader this attributes bindings-map))

       clojure.lang.IPersistentMap
       (~fn-name [~map-sym] (~fn-name ~(sexp/build-from-map map-sym clazz attributes bindings-map))))))

(defn- build-enum-definition [clazz fn-name protocol-name]
  (let [this (vary-meta (gensym "this") assoc :tag clazz)
        values (-> ^Class (eval clazz) .getEnumConstants vec)]
    `(extend-protocol ~protocol-name
       nil
       (~fn-name [_#] nil)

       ~clazz
       (~fn-name [~this]
         (reify
           ProtobufElement
           (proto-obj [_#] ~this)
           (mapify [_#] (case (.name ~this)
                          ~@(mapcat
                             #(let [val-name (.name ^Enum %)]
                                [val-name (keyword val-name)]) values)
                          nil))))

       clojure.lang.Keyword
       (~fn-name [kw#]
         (~fn-name (case kw#
                     ~@(mapcat
                        #(let [val-name (.name ^Enum %)]
                           [(keyword val-name) (symbol (name clazz) val-name)]) values)
                     nil))))))

(defmacro defprotos
  "Public macro. See tests for usage"
  [bindings-name root-clazz & bindings-seq]
  (let [named-bindings (->> bindings-seq
                            (partition-all 2))
        protobuf-classes (reflection/protobuf-classes root-clazz named-bindings)
        bindings-map (reduce (fn [m {:keys [name fn-name]}]
                               (assoc m (eval name) fn-name)) {} protobuf-classes)]
    `(do
       ~@(for [{:keys [name fn-name protocol-name]} protobuf-classes]
           `(defprotocol ~protocol-name
              (~fn-name [arg#])))
       ~@(for [{:keys [name fn-name protocol-name type]} protobuf-classes]
           (case type
             :message (build-proto-definition name fn-name protocol-name bindings-map)
             :enum (build-enum-definition name fn-name protocol-name)))
       (def ~bindings-name {:protobuf-mappers ~bindings-map}))))
