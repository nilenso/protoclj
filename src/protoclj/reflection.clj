(ns protoclj.reflection
  (:import [com.google.protobuf GeneratedMessage$Builder ByteString]
           [java.lang Iterable]))

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

(defn- internal-setter? [^Class type]
  "Check if the given class is part of an internal function"
  (or (= ByteString type)
      (= GeneratedMessage$Builder (.getSuperclass type))))

(defn- regular-attribute [kw read-interface builder-clazz ^java.lang.reflect.Method function type]
  "Build a map representing a regular attribute"
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

(defn- repeated-attribute [kw read-interface builder-clazz single-setter-name type]
  "Build a map representing a repeated attribute"
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

(defn  proto-attributes [clazz-sym]
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
