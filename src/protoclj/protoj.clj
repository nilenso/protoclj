(ns protoclj.protoj)
;; Pronounced Protege

(defprotocol ProtosMap
  (proto-get [m k])
  (proto-keys [m]))

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
         (map functions))))

(defn- keywordize-fn [^java.lang.reflect.Method fn]
  (->> fn
       .getName
       (map #(if (Character/isUpperCase %)
               (str "-" (clojure.string/lower-case %))
               %))
       (apply str)
       (#(clojure.string/replace % #"^get-" ""))
       keyword))

(defn- fetch-from-proto [this bindings ^java.lang.reflect.Method fn]
  (let [return-type (.getReturnType fn)
        base (list (symbol (str "." (.getName fn))) this)]
    (if-let [mapper (bindings return-type)]
      (list mapper base)
      base)))

(defn- define-proto [[clazz fn-name] bindings]
  (let [this (vary-meta (gensym "this") assoc :tag clazz)
        fns (->> clazz eval get-relevant-methods)]
    `(defn ~fn-name [~this]
       (reify ProtosMap
         (proto-get [_ k#]
           (case k#
             ~@(mapcat #(list (keywordize-fn %) (fetch-from-proto this bindings %)) fns)
             nil))
         (proto-keys [_] ~(vec (map keywordize-fn fns)))))))

(defmacro defproto-readers [_ & bindings-seq]
  (let [bindings (->> bindings-seq
                      (partition-all 2)
                      (map reverse)
                      (map vec)
                      (reduce (fn [m [clazz fn]] (assoc m (eval clazz) fn)) {}))]
    `(do ~@(map #(define-proto % bindings) bindings))))
