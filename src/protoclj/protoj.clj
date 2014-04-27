;; Pronounced Protege
(ns protoclj.protoj
  (:require [clojure.java.io])
  (:import [com.google.protobuf GeneratedMessage$Builder ByteString]
           [java.lang Iterable]))

(defprotocol ProtobufMap
  (proto-get [m k])
  (proto-get-raw [m k])
  (proto-keys [m])
  (proto-obj [m])
  (mapify [m]))

(extend-protocol ProtobufMap
  nil
  (proto-get [m k] nil)
  (proto-get-raw [m k] nil)
  (proto-keys [m] nil)
  (proto-obj [m] nil)
  (mapify [m] nil))

;; String Munging

(defn- keywordize-fn [^java.lang.reflect.Method fn]
  "Returns a keyword name from a function getFooBar -> :foo-bar"
  (->> fn
       .getName
       (map #(if (Character/isUpperCase ^Character %)
               (str "-" (clojure.string/lower-case %))
               %))
       (apply str)
       (#(clojure.string/replace % #"^get-|^set-" ""))
       keyword))

(defn- protocol-name [^Class class]
  "Random name for a protocol representing the class builder"
  (-> class
      .getName
      (clojure.string/replace #"[^a-zA-Z1-9]" "-")
      gensym))

;; Reflection and Fetching

(defn- internal-setter? [^Class type]
  "Check if the given class is part of an internal function"
  (or (= ByteString type)
      (= GeneratedMessage$Builder (.getSuperclass type))))

(defn- regular-attribute [kw read-interface builder-clazz ^java.lang.reflect.Method function type]
  "Build a map representing a regular attribute"
  {:keyword kw
   :reader (.getMethod ^Class read-interface
                       (clojure.string/replace (.getName function) #"^set" "get")
                       nil)
   :writer (.getMethod ^Class builder-clazz
                       (.getName function)
                       (into-array Class [type]))
   :type type
   :attribute-type :regular})

(defn- repeated-attribute [kw read-interface builder-clazz single-setter-name type]
  "Build a map representing a repeated attribute"
  (let [reader-name (-> single-setter-name
                        (clojure.string/replace #"set" "get")
                        (str "List"))
        setter-name (-> single-setter-name
                        (clojure.string/replace #"set" "addAll"))]
    {:keyword kw
     :reader (.getMethod ^Class read-interface reader-name nil)
     :writer (.getMethod ^Class builder-clazz setter-name (into-array Class [Iterable]))
     :type type
     :attribute-type :repeated}))

(defn- proto-attributes [clazz-sym]
  "Get a list of interesting attributes for the class.
  Currently done by seeing all setters. If it accepts 2 args, it's a repeated attr."
  (let [clazz ^Class (eval clazz-sym)
        read-interface ^Class
        (->> clazz
             .getInterfaces
             (filter #(.startsWith (.getName ^Class %) (.getName clazz)))
             first)
        builder-clazz ^Class (-> clazz (.getMethod "newBuilder" nil) .getReturnType)]
    (for [function (.getDeclaredMethods builder-clazz)
          :when (.startsWith (.getName function) "set")
          :let [kw (keywordize-fn function)
                param-types (.getParameterTypes function)
                type (last param-types)]
          :when (not (internal-setter? type))]
      (if (= 1 (count param-types))
        (regular-attribute kw read-interface builder-clazz function type)
        (repeated-attribute kw read-interface builder-clazz (.getName function) type)))))

;; sexp generation

(defmulti #^{:private true} fetch-as-object #(:attribute-type %3))

(defmethod fetch-as-object :regular [this bindings-map attribute]
  (let [sexp `(. ~this ~(symbol (.getName ^java.lang.reflect.Method (:reader attribute))))]
    (if-let [mapper (get bindings-map (:type attribute))]
      `(~mapper ~sexp)
      sexp)))

(defmethod fetch-as-object :repeated [this bindings-map attribute]
  (let [sexp `(. ~this ~(symbol (.getName ^java.lang.reflect.Method (:reader attribute))))]
    (if-let [mapper (get bindings-map (:type attribute))]
      `(vec (map ~mapper ~sexp))
      `(vec ~sexp))))

(defmulti #^{:private true} fetch-as-primitive #(:attribute-type %3))

(defmethod fetch-as-primitive :regular [this bindings-map attribute]
  (let [sexp `(. ~this ~(symbol (.getName ^java.lang.reflect.Method (:reader attribute))))]
    (if-let [mapper (get bindings-map (:type attribute))]
      `(mapify (~mapper ~sexp))
      sexp)))

(defmethod fetch-as-primitive :repeated [this bindings-map attribute]
  (let [sexp `(. ~this ~(symbol (.getName ^java.lang.reflect.Method (:reader attribute))))]
    (if-let [mapper (get bindings-map (:type attribute))]
      `(vec (map (comp mapify ~mapper) ~sexp))
      `(vec ~sexp))))

(defmulti #^{:private true} build-attribute-from-map #(:attribute-type %4))

(defmethod build-attribute-from-map :regular [map builder bindings-map {:keys [type] :as attribute}]
  (let [sexp `(~(:keyword attribute) ~map)
        sexp (if-let [mapper (get bindings-map type)]
               `(proto-obj (~mapper ~sexp))
               sexp)
        type-sym (vary-meta (gensym "val") assoc :tag (-> ^Class type .getName symbol))]
    `(when-let [~type-sym ~sexp]
       (. ~builder ~(symbol (.getName ^java.lang.reflect.Method (:writer attribute))) ~type-sym))))

(defmethod build-attribute-from-map :repeated [map builder bindings-map {:keys [type] :as attribute}]
  (let [sexp `(~(:keyword attribute) ~map)
        sexp (if-let [mapper (get bindings-map type)]
               `(map (comp proto-obj ~mapper) ~sexp)
               sexp)]
    `(. ~builder ~(symbol (.getName ^java.lang.reflect.Method (:writer attribute))) ~sexp)))

;; Magic

(defn- build-reader [this attributes bindings-map]
  "The main reify that can get different params"
  `(reify
     ProtobufMap
     (proto-get [_# k#]
       (case k#
         ~@(mapcat #(list (:keyword %) (fetch-as-object this bindings-map %)) attributes)
         nil))
     (proto-get-raw [_# k#]
       (case k#
         ~@(mapcat #(list (:keyword %) (fetch-as-object this nil %)) attributes)
         nil))
     (proto-keys [_#] ~(vec (map :keyword attributes)))
     (proto-obj [_#] ~this)
     (mapify [a#]
       (hash-map
        ~@(mapcat #(list (:keyword %) (fetch-as-primitive this bindings-map %)) attributes)))

     clojure.java.io.IOFactory
     (make-input-stream [x# opts#]
       (clojure.java.io/make-input-stream (.toByteArray ~this) opts#))
     (make-output-stream [x# opts#]
       (throw (IllegalArgumentException. (str "Cannot create an output stream from protobuf"))))
     (make-reader [x# opts#]
       (clojure.java.io/make-reader (clojure.java.io/make-input-stream x# opts#) opts#))
     (make-writer [x# opts#]
       (clojure.java.io/make-writer (clojure.java.io/make-output-stream x# opts#) opts#))))

(defn- build-from-map [map clazz attributes bindings-map]
  "Generate the reify from a map"
  (let [builder (gensym "builder")]
    `(let [~builder (. ~clazz ~'newBuilder)]
       ~@(for [attribute attributes]
           (build-attribute-from-map map builder bindings-map attribute))
       (.build ~builder))))

(defn- build-proto-definition [[clazz fn-name] bindings-map]
  "Returns a sexp for defining the proto"
  (let [this (vary-meta (gensym "this") assoc :tag clazz)
        attributes (proto-attributes clazz)
        protocol-name (-> clazz eval protocol-name)
        byte-array-type (-> 0 byte-array class .getName symbol)
        byte-array-symbol (vary-meta (gensym "stream") assoc :tag byte-array-type)
        map-sym (gensym "map")]
    `(do
       (defprotocol ~protocol-name
         (~fn-name [~this]))
       (extend-protocol ~protocol-name
         nil
         (~fn-name [_#] nil)

         ~byte-array-type
         (~fn-name [~byte-array-symbol] (~fn-name (. ~clazz ~'parseFrom ~byte-array-symbol)))

         java.io.InputStream
         (~fn-name [input-stream#] (~fn-name (. ~clazz ~'parseFrom input-stream#)))

         ~clazz
         (~fn-name [~this] ~(build-reader this attributes bindings-map))

         clojure.lang.IPersistentMap
         (~fn-name [~map-sym] (~fn-name ~(build-from-map map-sym clazz attributes bindings-map)))))))

(defmacro defprotos [bindings-name & bindings-seq]
  "Public macro. See tests for usage"
  (let [bindings (->> bindings-seq
                      (partition-all 2)
                      (map reverse)
                      (map vec))
        bindings-map (reduce (fn [m [clazz fn]] (assoc m (eval clazz) fn)) {} bindings)]
    `(do
       ~@(for [binding bindings]
           (build-proto-definition binding bindings-map))
       (def ~bindings-name {:protobuf-mappers ~bindings-map}))))
