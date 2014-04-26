;; Pronounced Protege
(ns protoclj.protoj)

(defprotocol ProtobufMap
  (proto-get [m k])
  (proto-keys [m])
  (proto-obj [m]))

(defn- get-relevant-methods [^Class clazz]
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

(defn- keywordize-fn [^java.lang.reflect.Method fn]
  (->> fn
       .getName
       (map #(if (Character/isUpperCase ^Character %)
               (str "-" (clojure.string/lower-case %))
               %))
       (apply str)
       (#(clojure.string/replace % #"^get-" ""))
       keyword))

(defn- fetch-from-proto [this bindings-map ^java.lang.reflect.Method fn]
  (let [return-type (.getReturnType fn)
        base (list (symbol (str "." (.getName fn))) this)]
    (if-let [mapper (bindings-map return-type)]
      (list mapper base)
      base)))

(defn- define-proto [[clazz fn-name] bindings-map]
  (let [this (vary-meta (gensym "this") assoc :tag clazz)
        fns (->> clazz eval get-relevant-methods)]
    `(defn ~fn-name [~this]
       (reify ProtobufMap
         (proto-get [_ k#]
           (case k#
             ~@(mapcat #(list (keywordize-fn %) (fetch-from-proto this bindings-map %)) fns)
             nil))
         (proto-keys [_] ~(vec (map keywordize-fn fns)))
         (proto-obj [_] ~this)))))

(defmacro defprotos [bindings-name & bindings-seq]
  (let [bindings (->> bindings-seq
                      (partition-all 2)
                      (map reverse)
                      (map vec))
        bindings-map (reduce (fn [m [clazz fn]] (assoc m (eval clazz) fn)) {} bindings)]
    `(do ~@(map #(define-proto % bindings-map) bindings)
         (def ~bindings-name ~bindings-map))))

(defn ->map [bindings-map object]
  (persistent!
   (reduce #(assoc! %1 %2 (proto-get object %2))
           (transient {})
           (proto-keys object))))
