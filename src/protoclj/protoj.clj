;; Pronounced Protege
(ns protoclj.protoj
  (:require [clojure.java.io])
  (:import [com.google.protobuf GeneratedMessage$Builder ByteString]
           [java.lang Iterable]))

(defprotocol ProtobufMap
  (proto-get [m k])
  (proto-get-raw [m k])
  (proto-keys [m])
  (proto-obj [m]))

(extend-protocol ProtobufMap
  nil
  (proto-get [m k] nil)
  (proto-get-raw [m k] nil)
  (proto-keys [m] nil)
  (proto-obj [m] nil))

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
   :attribute-type :regular
   :reverse-mapping (fn [sexp mapper]
                      `(proto-obj (~mapper ~sexp)))})

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
     :attribute-type :repeated
     :reverse-mapping (fn [sexp mapper]
                        `(map (comp proto-obj ~mapper) ~sexp))}))

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

(defmulti #^{:private true} fetch-from-proto #(:attribute-type %3))

(defmethod fetch-from-proto :regular [this bindings-map attribute]
  (let [sexp `(. ~this ~(symbol (.getName ^java.lang.reflect.Method (:reader attribute))))]
    (if-let [mapper (get bindings-map (:type attribute))]
      `(~mapper ~sexp)
      sexp)))

(defmethod fetch-from-proto :repeated [this bindings-map attribute]
  (let [sexp `(. ~this ~(symbol (.getName ^java.lang.reflect.Method (:reader attribute))))]
    (if-let [mapper (get bindings-map (:type attribute))]
      `(vec (map ~mapper ~sexp))
      `(vec ~sexp))))

;; Magic

(defn- build-reader [this attributes bindings-map]
  "The main reify that can get different params"
  `(reify
     ProtobufMap
     (proto-get [_# k#]
       (case k#
         ~@(mapcat #(list (:keyword %) (fetch-from-proto this bindings-map %)) attributes)
         nil))
     (proto-get-raw [_# k#]
       (case k#
         ~@(mapcat #(list (:keyword %) (fetch-from-proto this nil %)) attributes)
         nil))
     (proto-keys [_#] ~(vec (map :keyword attributes)))
     (proto-obj [_#] ~this)

     clojure.java.io.IOFactory
     (make-input-stream [x# opts#]
       (clojure.java.io/make-input-stream (.toByteArray ~this) opts#))
     (make-output-stream [x# opts#]
       (throw (IllegalArgumentException. (str "Cannot create an output stream from protobuf"))))
     (make-reader [x# opts#]
       (clojure.java.io/make-reader (clojure.java.io/make-input-stream x# opts#) opts#))
     (make-writer [x# opts#]
       (clojure.java.io/make-writer (clojure.java.io/make-output-stream x# opts#) opts#))))

(defn- build-from-map [map clazz attributes binding-map]
  "Generate the reify from a map"
  (let [builder (gensym "builder")]
    `(let [~builder (. ~clazz ~'newBuilder)]
       ~@(for [attribute attributes
               :let [fn ^java.lang.reflect.Method (:writer attribute)
                     type ^Class (:type attribute)
                     fn-symbol (symbol (str "." (.getName fn)))
                     sexp `(~(:keyword attribute) ~map)
                     sexp (if-let [mapper (get binding-map type)]
                            ((:reverse-mapping attribute) sexp mapper)
                            sexp)
                     type-sym (vary-meta (gensym "val") assoc :tag (-> type .getName symbol))]]
           `(when-let [~type-sym ~sexp]
              (~fn-symbol ~builder ~type-sym)))
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

;; This is the only place where some amount of runtime reflection happens.
;; I really hope this is not slow
(defn ->map [{:keys [protobuf-mappers] :as bindings-map} object]
  "Turn a ProtoMap into a map"
  (persistent!
   (reduce (fn [map key]
             (assoc! map key
                     (let [result (proto-get-raw object key)]
                       (if (instance? Iterable result)
                         (if-let [mapping (protobuf-mappers (class (first result)))]
                           (reduce #(conj %1 (->map bindings-map (mapping %2))) [] result)
                           (vec result))
                         (if-let [mapping (protobuf-mappers (class result))]
                           (->map bindings-map (mapping result))
                           result)))))
           (transient {})
           (proto-keys object))))
