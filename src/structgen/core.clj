(ns structgen.core
  (:import
    [java.text SimpleDateFormat]
    [java.util Date])
  (:require
    [gloss [core :as gc] [io :as gio]]
    [clojure.data.dependency :as dep]))

(def ^:dynamic *config* {:le true :align 4})

(defmacro with-config
  [config & body]
  `(binding [*config* (merge *config* config)]
     (do ~@body)))

(defprotocol StructElement
  (cname [this]
    "Returns an element's C type name.")
  (compile-type [this]
    "Returns an element's compiled gloss frame.
    Where applicable chooses endianess based on current `*config*` setting.")
  (element-type [this]
    "Returns the element type.")
  (length [this]
    "Returns the element's array length (zero for single values).")
  (primitive? [this]
    "Returns true if element is not a struct.")
  (sizeof [this]
    "Returns byte size of compiled element.")
  (template [this] [this all?]
    "Returns the element's data template: map for structs, vector for arrays"))

(defprotocol Struct
  (encode [this m]
    "Encodes the map `m` into a byte buffer. Missing values are replaced
    with values from the template.")
  (decode [this bytes]
    "Decodes a byte buffer into map with structure defined by the struct.")
  (dependencies [this] [this g]
    "Returns the graph of all transitive dependencies for this struct.
    Use clojure.data.dependency for further analysis.")
  (struct-spec [this]
    "Returns a map of field names and StructElement or Struct implementations
    for the given struct.")
  (gen-source* [this]
    "Generates C source for the typedef of this struct.")
  (gen-source [this]
    "Generates C source for the typedef of this struct and all required
    dependencies (in order)."))

(defn primitive*
  "Returns a StructElement implementation for the given primitive type description." 
  [cname type size]
  (reify
    StructElement
    (cname [this] cname)
    (compile-type [this]
      (gc/compile-frame
        (if (> size 1)
          (keyword (str (name type) (if (:le *config*) "-le" "-be")))
          type)))
    (element-type [this] type)
    (length [this] 0)
    (primitive? [this] true)
    (sizeof [this] size)
    (template [this] 0)
    (template [this _] 0)))

(defn element-array*
  "Returns a StructElement implementation for the given element array description."
  [e len]
  (reify
    StructElement
    (cname [this] (cname e))
    (compile-type [this]
      (gc/compile-frame (repeat len (compile-type e))))
    (element-type [this] e)
    (length [this] len)
    (primitive? [this] (primitive? e))
    (sizeof [this] (* len (sizeof e)))
    (template [this] (template this false))
    (template [this all?] (vec (repeat len (template e all?))))))

(defn make-registry
  "Builds a vanilla type registry populated only with primitive types."
  [& specs]
  (ref {:char (primitive* "char" :byte 1)
        :uchar (primitive* "unsigned char" :ubyte 1)
        :short (primitive* "short" :int16 2)
        :ushort (primitive* "unsigned short" :uint16 2)
        :int (primitive* "int" :int32 4)
        :uint (primitive* "unsigned int" :uint32 4)
        :float (primitive* "float" :float32 4)
        :double (primitive* "double" :float64 8)}))

(def ^:dynamic *registry* (make-registry))
(declare make-struct)

(defn register!
  "Registers a pre-build struct with the given id or builds and registers
  a number of structs for the given specs. Throws IllegalArgumentException
  for duplicate IDs. Each spec is a vector of: [typename & fields]

  Examples:

      (register! :Foo (make-struct 'Foo [:a :float 4] [:b :int]))

      (register!
        [[:Vec3 [:x :float] [:y :float] [:z :float]]
         [:Camera [:pos :Vec3] [:target :Vec3] [:up :Vec3]]])"
  ([specs]
    (doseq [[id & fields] specs]
      (register! id (apply make-struct id fields)))
    true)
  ([id type]
    (if (id @*registry*)
      (throw (IllegalArgumentException. "duplicate type"))
      (dosync (alter *registry* assoc id type)))
    true))

(defn registed-type
  [id]
  (get @*registry* id))

;; clojure.contrib.map-utils - chouser
(defn deep-merge-with
  "Like merge-with, but merges maps recursively, applying the given fn
  only when there's a non-map at a particular level.

  (deep-merge-with +
    {:a {:b {:c 1 :d {:x 1 :y 2}} :e 3} :f 4}
    {:a {:b {:c 2 :d {:z 9} :z 3} :e 100}})
  -> {:a {:b {:z 3, :c 3, :d {:z 9, :x 1, :y 2}}, :e 103}, :f 4}"
  [f & maps]
  (apply
    (fn m [& maps]
      (if (every? map? maps)
        (apply merge-with m maps)
        (apply f maps)))
    maps))

(defn deep-select-keys
  [m struct]
  (reduce
    (fn [acc [k v]]
      (if-let [ks (k (struct-spec struct))]
        (assoc acc k 
          (cond
            (map? v)
              (deep-select-keys v ks)
            (and (sequential? v) (not (primitive? ks)))
              (map #(deep-select-keys % (element-type ks)) v)
            :default v))
        acc))
    {} m))

(defn merge-with-template
  [tpl data]
  (cond
    (sequential? tpl)
    (let [ct (count tpl) cd (count data)]
      (if (every? map? [(first tpl) (first data)])
        (map (fn [a b] (deep-merge-with merge-with-template a b))
             tpl (concat (take ct data) (drop cd tpl)))
        (if (< cd ct)
          (concat data (drop cd tpl))
          (take ct data))))
    (nil? data) tpl
    :default data))

(defn ceil-multiple-of
  "Rounds up `b` to a multiple of `a`."
  [a b]
  (let [r (rem b a)] (if (zero? r) b (- (+ b a) r))))

(defn build-align-spec*
  [gap]
  (when (pos? gap)
    {:gid (keyword (gensym "sg__"))
     :gcode (gc/compile-frame (repeat gap :byte))
     :gdata (vec (repeat gap 0))}))

(defn build-spec*
  [fields]
  (map
    (fn [[id t len]]
      (if-let [e (get @*registry* t)]
        (let [e (if (and len (pos? len)) (element-array* e len) e)
              s (sizeof e)
              align (:align *config*)
              stride (if (pos? align) (ceil-multiple-of align s) s)
              gap (- stride s)]
          (merge {:id id :element e} (build-align-spec* gap)))
        (throw (IllegalArgumentException. (str "unknown type " t)))))
    fields))

(defn build-template*
  [spec]
  (apply merge
    (concat
      (map (fn [{:keys [id element]}] {id (template element true)}) spec)
      (map (fn [{:keys [gid gdata]}] {gid gdata}) (filter :gid spec)))))

(defn build-codec*
  [spec]
  (reduce
    (fn [acc {:keys [id element gid gcode]}]
      (if gid
        (conj acc id (compile-type element) gid gcode)
        (conj acc id (compile-type element))))
    [] spec))

(defn- header
  []
  (str
    "/* generated @ "
    (.format (SimpleDateFormat. "yyyy-MM-dd HH:mm:ss") (Date.))
    " by com.postspectacular/structgen */\n\n"))

(defn make-struct
  [tname & fields]
  (let [spec (build-spec* fields)
        tpl (build-template* spec)
        codec (build-codec* spec)
        frame (gc/compile-frame (apply gc/ordered-map codec))
        size (gc/sizeof frame)
        spec-map (reduce (fn [acc {:keys[id element]}] (assoc acc id element)) {} spec)]
    (reify
      StructElement
      (cname [this] (name tname))
      (compile-type [this] frame)
      (length [this] 0)
      (primitive? [this] false)
      (sizeof [this] size)
      (template [this] (template this false))
      (template [this all?]
        (if all? tpl (deep-select-keys tpl this)))
      Struct
      (encode [this m]
        (gio/encode frame (deep-merge-with merge-with-template tpl m)))
      (decode [this bytes]
        (gio/decode frame bytes))
      (dependencies [this]
        (dependencies this (dep/graph)))
      (dependencies [this g]
        (let [deps (into #{}
                     (map
                       #(if (pos? (length %)) (element-type %) %)
                       (filter #(not (primitive? %)) (vals spec-map))))
              g (reduce (fn [g t] (dependencies t g)) g deps)]
          (reduce (fn [g t] (dep/depend g this t)) g deps)))
      (gen-source* [this]
        (apply str
          "typedef struct {\n"
          (concat
            (mapcat
              (fn [{:keys [id element]}]
                (let [[id cn len] [(name id) (cname element) (length element)]]
                  (if (pos? (length element)) 
                    ["  " cn " " id "[" len "];\n"]
                    ["  " cn " " id ";\n"])))
              spec)
            ["} " (name tname) ";\n\n"])))
      (gen-source [this]
        (let [g (dependencies this)
              deps (dep/transitive-dependencies g this)
              sorted-deps (vec (dep/topo-sort g (vec (into #{this} deps))))]
          (apply str (header) (map gen-source* sorted-deps))))
      (struct-spec [this] spec-map))))
