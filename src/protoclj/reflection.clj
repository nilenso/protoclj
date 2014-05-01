(ns protoclj.reflection
  (:import [com.google.protobuf GeneratedMessage GeneratedMessage$Builder ByteString]
           [java.lang Iterable]))

(defn- keywordize-fn
  "Returns a keyword name from a function getFooBar -> :foo-bar"
  [^java.lang.reflect.Method fn]
  (->> fn
       .getName
       (map #(if (Character/isUpperCase ^Character %)
               (str "-" (clojure.string/lower-case %))
               %))
       (apply str)
       (#(clojure.string/replace % #"^get-|^set-" ""))
       keyword))

(defn- internal-setter?
  "Check if the given class is part of an internal function"
  [^Class type]
  (or (= ByteString type)
      (= GeneratedMessage$Builder (.getSuperclass type))))

(defn- regular-attribute
  "Build a map representing a regular attribute"
  [kw read-interface builder-clazz ^java.lang.reflect.Method function type]
  {:name-kw kw
   :reader (.getMethod ^Class read-interface
                       (clojure.string/replace (.getName function) #"^set" "get")
                       nil)
   :presence (.getMethod ^Class read-interface
                         (clojure.string/replace (.getName function) #"^set" "has")
                         nil)
   :writer (.getMethod ^Class builder-clazz
                       (.getName function)
                       (into-array Class [type]))
   :type type
   :attribute-type :regular})

(defn- repeated-attribute
  "Build a map representing a repeated attribute"
  [kw read-interface builder-clazz single-setter-name type]
  (let [reader-name (-> single-setter-name
                        (clojure.string/replace #"set" "get")
                        (str "List"))
        setter-name (-> single-setter-name
                        (clojure.string/replace #"set" "addAll"))]
    {:name-kw kw
     :reader (.getMethod ^Class read-interface reader-name nil)
     :writer (.getMethod ^Class builder-clazz setter-name (into-array Class [Iterable]))
     :type type
     :attribute-type :repeated}))

(defn proto-attributes
  "Get a list of interesting attributes for the class.
  Currently done by seeing all setters.
  If it accepts 2 args, it's a repeated attr."
  [clazz-sym]
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

(defn- iterate-inner-classes [^Class root]
  (lazy-cat (cons root (mapcat iterate-inner-classes (.getDeclaredClasses root)))))

(defn- anonymous-protocol [class-name]
  (-> class-name
      name
      (clojure.string/replace #"[^a-zA-Z1-9]" "-")
      gensym))

(defn- anonymous-symbol [class-name]
  (-> class-name
      name
      (clojure.string/replace #"[^a-zA-Z1-9]" "-")
      clojure.string/lower-case
      gensym))

(defn protobuf-classes
  "Get a map of classes to the function and protocol that represent it"
  [root-clazz named-bindings-seq]
  (let [named-bindings (reduce (fn [m [fn clazz]]
                                 (assoc m (symbol (.getName ^Class (eval clazz))) fn))
                               {} named-bindings-seq)
        classes (->> root-clazz
                     eval
                     iterate-inner-classes)]

    (for [^Class clazz classes
          :let [enum? (.isEnum clazz)
                message? (= GeneratedMessage (.getSuperclass clazz))]
          :when (or message? enum?)
          :let [class-sym (symbol (.getName clazz))]]
      {:name class-sym
       :fn-name (get named-bindings class-sym (anonymous-symbol class-sym))
       :protocol-name (anonymous-protocol class-sym)
       :type (if enum? :enum :message)})))
