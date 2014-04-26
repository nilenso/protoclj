;; Pronounced Protege
(ns protoclj.protoj
  (:require [clojure.java.io])
  (:import [com.google.protobuf
            GeneratedMessage$Builder
            ByteString]))

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
  (or (= ByteString type)
      (= GeneratedMessage$Builder (.getSuperclass type))))

(defn- proto-attributes [clazz-sym]
  (let [clazz ^Class (eval clazz-sym)
        read-interface ^Class
        (->> clazz
             .getInterfaces
             (filter #(.startsWith (.getName ^Class %) (.getName clazz)))
             first)
        builder-clazz ^Class (-> clazz (.getMethod "newBuilder" nil) .getReturnType)]
    (for [function (.getDeclaredMethods builder-clazz)
          :when (.startsWith (.getName function) "set")
          :let [param-types (.getParameterTypes function)
                type (last param-types)]
          :when (not (internal-setter? type))
          :let [reader-fn (.getMethod read-interface (clojure.string/replace (.getName function) #"^set" "get") nil)
                val-type (.getReturnType reader-fn)
                writer-fn (.getMethod builder-clazz (.getName function) (into-array Class [val-type]))]]
      {:keyword (keywordize-fn reader-fn)
       :reader reader-fn
       :writer writer-fn})))

(defn- fetch-from-proto [this bindings-map attribute]
  "Returns a sexp that can fetch the key from the protobuf.
  Pass in nil to bindings-map to circumvent nested translation"
  (let [fn ^java.lang.reflect.Method (:reader attribute)
        return-type (.getReturnType fn)
        base (list (symbol (str "." (.getName fn))) this)]
    (if-let [mapper (get bindings-map return-type)]
      (list mapper base)
      base)))

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
                     [^Class type] (.getParameterTypes fn)
                     fn-symbol (symbol (str "." (.getName fn)))
                     kw (:keyword attribute)
                     body (if-let [mapper (get binding-map type)]
                            `(proto-obj (~mapper (~kw ~map)))
                            (list kw map))
                     type-sym (vary-meta (gensym "val") assoc :tag (-> type .getName symbol))]]
           `(when-let [~type-sym ~body]
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

(defn ->map [{:keys [protobuf-mappers] :as bindings-map} object]
  "Turn a ProtoMap into a map"
  (persistent!
   (reduce #(assoc! %1 %2
                    (let [result (proto-get-raw object %2)]
                      (if-let [mapping (protobuf-mappers (class result))]
                        (->map bindings-map (mapping result))
                        result)))
           (transient {})
           (proto-keys object))))
