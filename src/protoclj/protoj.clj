;; Pronounced Protege
(ns protoclj.protoj
  (:require [clojure.java.io]))

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

;; Reflection and Fetching

(defn- get-reader-methods [clazz-sym]
  "Returns a list of all protobuf related functions for a class"
  (let [^Class clazz (eval clazz-sym)
        interface (->> clazz
                       .getInterfaces
                       (filter #(.startsWith (.getName ^Class %) (.getName clazz)))
                       first)
        functions (reduce (fn [m ^java.lang.reflect.Method method]
                            (assoc m (.getName method) method))
                          nil (.getMethods ^Class interface))]
    (->> functions
         keys
         (filter #(.startsWith ^String % "has"))
         (map #(clojure.string/replace % #"^has" "get"))
         (remove #{"getField"})
         (map functions))))

(defn- get-writer-methods [clazz-sym]
  "Returns a list of methods on the builder"
  (let [^Class clazz (eval clazz-sym)
        readers (get-reader-methods clazz-sym)
        builder-clazz (-> clazz (.getMethod "newBuilder" nil) .getReturnType)]
    (for [reader readers
          :let [setter-name (-> reader .getName (clojure.string/replace #"^get" "set"))
                type (.getReturnType reader)]]
      (.getMethod builder-clazz setter-name (into-array Class [type])))))

(defn- fetch-from-proto [this bindings-map ^java.lang.reflect.Method fn]
  "Returns a sexp that can fetch the key from the protobuf.
  Pass in nil to bindings-map to circumvent nested translation"
  (let [return-type (.getReturnType fn)
        base (list (symbol (str "." (.getName fn))) this)]
    (if-let [mapper (get bindings-map return-type)]
      (list mapper base)
      base)))

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

;; Magic

(defn- build-reader [this fns bindings-map]
  "The main reify that can get different params"
  `(reify
     ProtobufMap
     (proto-get [_# k#]
       (case k#
         ~@(mapcat #(list (keywordize-fn %) (fetch-from-proto this bindings-map %)) fns)
         nil))
     (proto-get-raw [_# k#]
       (case k#
         ~@(mapcat #(list (keywordize-fn %) (fetch-from-proto this nil %)) fns)
         nil))
     (proto-keys [_#] ~(vec (map keywordize-fn fns)))
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

(defn- build-from-map [map clazz binding-map]
  "Generate the reify from a map"
  (let [builder (gensym "builder")]
    `(let [~builder (. ~clazz ~'newBuilder)]
       ~@(for [^java.lang.reflect.Method fn (get-writer-methods clazz)
               :let [[^Class type] (.getParameterTypes fn)
                     fn-symbol (symbol (str "." (.getName fn)))
                     kw (keywordize-fn fn)
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
         (~fn-name [~this] ~(build-reader this (get-reader-methods clazz) bindings-map))

         clojure.lang.IPersistentMap
         (~fn-name [~map-sym] (~fn-name ~(build-from-map map-sym clazz bindings-map)))))))

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
