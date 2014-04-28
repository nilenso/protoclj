(ns protoclj.sexp)

(defmulti fetch-as-object #(:attribute-type %3))

(defmethod fetch-as-object :regular [this bindings-map {:keys [reader presence type]}]
  (let [sexp `(. ~this ~(symbol (.getName ^java.lang.reflect.Method reader)))
        sexp (if-let [mapper (get bindings-map type)]
               `(~mapper ~sexp)
               sexp)]
    `(when (. ~this ~(symbol (.getName ^java.lang.reflect.Method presence)))
       ~sexp)))

(defmethod fetch-as-object :repeated [this bindings-map {:keys [reader type]}]
  (let [sexp `(. ~this ~(symbol (.getName ^java.lang.reflect.Method reader)))]
    (if-let [mapper (get bindings-map type)]
      `(vec (map ~mapper ~sexp))
      `(vec ~sexp))))

(defmulti fetch-as-primitive #(:attribute-type %3))

(defmethod fetch-as-primitive :regular [this bindings-map {:keys [reader type presence]}]
  (let [sexp `(. ~this ~(symbol (.getName ^java.lang.reflect.Method reader)))
        sexp (if-let [mapper (get bindings-map type)]
               `(protoclj.core/mapify (~mapper ~sexp))
               sexp)]
    `(when (. ~this ~(symbol (.getName ^java.lang.reflect.Method presence)))
       ~sexp)))

(defmethod fetch-as-primitive :repeated [this bindings-map {:keys [reader type]}]
  (let [sexp `(. ~this ~(symbol (.getName ^java.lang.reflect.Method reader)))]
    (if-let [mapper (get bindings-map type)]
      `(vec (map (comp protoclj.core/mapify ~mapper) ~sexp))
      `(vec ~sexp))))

(defmulti build-attribute-from-map #(:attribute-type %4))

(defmethod build-attribute-from-map :regular [map builder bindings-map {:keys [type writer name-kw] :as attribute}]
  (let [sexp `(~name-kw ~map)
        sexp (if-let [mapper (get bindings-map type)]
               `(protoclj.core/proto-obj (~mapper ~sexp))
               sexp)
        type-sym (vary-meta (gensym "val") assoc :tag (-> ^Class type .getName symbol))]
    `(when-let [~type-sym ~sexp]
       (. ~builder ~(symbol (.getName ^java.lang.reflect.Method writer)) ~type-sym))))

(defmethod build-attribute-from-map :repeated [map builder bindings-map {:keys [type name-kw] :as attribute}]
  (let [sexp `(~name-kw ~map)
        sexp (if-let [mapper (get bindings-map type)]
               `(map (comp protoclj.core/proto-obj ~mapper) ~sexp)
               sexp)]
    `(. ~builder ~(symbol (.getName ^java.lang.reflect.Method (:writer attribute))) ~sexp)))

(defn build-from-map [map clazz attributes bindings-map]
  "Generate the reify from a map"
  (let [builder (gensym "builder")]
    `(let [~builder (. ~clazz ~'newBuilder)]
       ~@(for [attribute attributes]
           (build-attribute-from-map map builder bindings-map attribute))
       (.build ~builder))))
