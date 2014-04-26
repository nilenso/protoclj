;; Pronounced Protege
(ns protoclj.protoj
  (:require [clojure.java.io]))

(defprotocol ProtobufMap
  (proto-get [m k])
  (proto-get-raw [m k])
  (proto-keys [m])
  (proto-obj [m]))

;; Reflection and Fetching

(defn- get-relevant-methods [^Class clazz]
  "Returns a list of all protobuf related functions for a class"
  (let [interface (->> clazz
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
       (#(clojure.string/replace % #"^get-" ""))
       keyword))

(defn- protocol-name [^Class class]
  (-> class
      .getName
      (clojure.string/replace #"[^a-zA-Z1-9]" "-")
      gensym))

;; Magic

(defn- define-proto [[clazz fn-name] bindings-map]
  "Returns a sexp for defining the proto"
  (let [this (vary-meta (gensym "this") assoc :tag clazz)
        fns (-> clazz eval get-relevant-methods)
        protocol-name (-> clazz eval protocol-name)
        byte-array-type (-> 0 byte-array class .getName symbol)
        byte-array-symbol (vary-meta (gensym "stream") assoc :tag byte-array-type)]
    `(do
       (defprotocol ~protocol-name
         (~fn-name [~this]))
       (extend-protocol ~protocol-name
         nil
         (~fn-name [_#] nil)

         ~byte-array-type
         (~fn-name [~byte-array-symbol] (~fn-name (. ~clazz parseFrom ~byte-array-symbol)))

         ~clazz
         (~fn-name [~this]
           (reify ProtobufMap
             (proto-get [_# k#]
               (case k#
                 ~@(mapcat #(list (keywordize-fn %) (fetch-from-proto this bindings-map %)) fns)
                 nil))
             (proto-get-raw [_# k#]
               (case k#
                 ~@(mapcat #(list (keywordize-fn %) (fetch-from-proto this nil %)) fns)
                 nil))
             (proto-keys [_#] ~(vec (map keywordize-fn fns)))
             (proto-obj [_#] ~this)))))))

(defmacro defprotos [bindings-name & bindings-seq]
  "Public macro. See tests for usage"
  (let [bindings (->> bindings-seq
                      (partition-all 2)
                      (map reverse)
                      (map vec))
        bindings-map (reduce (fn [m [clazz fn]] (assoc m (eval clazz) fn)) {} bindings)]
    `(do ~@(map #(define-proto % bindings-map) bindings)
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
