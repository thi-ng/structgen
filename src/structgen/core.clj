(ns structgen.core
  "Utilities to seamlessly work with native C structs.
  Provides a C typedef parser and extensible struct generators
  for converting between Clojure maps and memory aligned
  byte buffers. Supports nested structs, arrays, custom C
  primitives (e.g as used by OpenCL) and endianess.
  Can generate C source from Clojure defined structs."
  ^{:author "Karsten Schmidt"}
  (:import
    [java.text SimpleDateFormat]
    [java.util Date]
    [java.nio ByteBuffer])
  (:require
    [gloss [core :as gc] [io :as gio]]
    [clojure.data.dependency :as dep]))

(def ^:dynamic *config* {:le true :align 4})

(defmacro with-config
  [config & body]
  `(binding [*config* (merge *config* ~config)]
     (do ~@body)))

(defprotocol StructElement
  "Defines common functionality for primitives, primitive vectors,
  arrays and composite types (structs)."
  (cdeclare [this id]
    "Returns the C declaration for this element and given symbol id.
    E.g. `float x[100];`")
  (cname [this]
    "Returns the element's C type name.")
  (compile-type [this]
    "Returns an element's compiled gloss frame.
    Where applicable chooses endianess based on current `*config*` setting.")
  (element-type [this]
    "Returns the element type `StructField` implementation.")
  (length [this]
    "Returns the element's array length (zero for single values).")
  (primitive? [this]
    "Returns true if element is not a struct.")
  (sizeof [this]
    "Returns byte size of compiled element.")
  (template [this] [this all?]
    "Returns the element's data template: map for structs, vector for arrays"))

(defprotocol Struct
  "Defines functionality only available for composite types (structs)."
  (encode ^ByteBuffer [this m]
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
    "Builds a dependency graph and generates C source for the typedef of
    this struct and all required dependencies (in correct order)."))

(defn primitive*
  "Returns a `StructElement` implementation for the given primitive type description:

      cname - C type name
      type - gloss codec type
      size - size in bytes" 
  [cname type size]
  (reify
    StructElement
    (cdeclare [this id] (str cname " " id ";"))
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

(defn primitive-vec*
  "Returns a `StructElement` implementation for the given vector primitive type description.
  E.g. OpenCL extends C with vector prims like `float4` which technically are like arrays,
  but differ in their C declarations:
  
      float x[4]; // 4-element C array
      float4 x; // float4 primitive

  Function arguments:

      cname - C type name
      type - StructElement implementation of this vectors element type
      len - number of elements

  Example:

      (primitive-vec* \"char2\" (lookup :char) 2)"
  [cname type len]
  (reify
    StructElement
    (cdeclare [this id] (str cname " " id ";"))
    (cname [this] cname)
    (compile-type [this]
      (gc/compile-frame (repeat len (compile-type type))))
    (element-type [this] type)
    (length [this] len)
    (primitive? [this] true)
    (sizeof [this] (* len (sizeof type)))
    (template [this] (template this true))
    (template [this _] (vec (repeat len 0)))))

(defn element-array*
  "Returns a `StructElement` implementation for the given element array description.
  Arrays can be created for any type (primitives, primitive vecs, structs). Currently
  only supports 1-dimensional arrays."
  ([e len] (element-array* e len (cname e)))
  ([e len cname]
    (reify
      StructElement
      (cdeclare [this id] (str cname " " id "[" len "];"))
      (cname [this] cname)
      (compile-type [this]
        (gc/compile-frame (repeat len (compile-type e))))
      (element-type [this] e)
      (length [this] len)
      (primitive? [this] (primitive? e))
      (sizeof [this] (* len (sizeof e)))
      (template [this] (template this false))
      (template [this all?] (vec (repeat len (template e all?)))))))

(defn make-registry
  "Builds a vanilla type registry populated only with primitive types."
  [& specs]
  {:char (primitive* "char" :byte 1)
   :uchar (primitive* "unsigned char" :ubyte 1)
   :short (primitive* "short" :int16 2)
   :ushort (primitive* "unsigned short" :uint16 2)
   :int (primitive* "int" :int32 4)
   :uint (primitive* "unsigned int" :uint32 4)
   :float (primitive* "float" :float32 4)
   :double (primitive* "double" :float64 8)})

(def ^:dynamic *registry* (ref (make-registry)))

(defn reset-registry!
  ([] (reset-registry! nil))
  ([r] (dosync (ref-set *registry* (or r (make-registry))))))

(defmacro with-registry
  "Binds the type `*registry*` to the supplied map and executes body
  in a `do` form. The given registry value is automatically wrapped
  in or converted to a ref (if not already).
  If the custom registry is supposed to be used outside the macro
  scope at a later time, it **must** already be wrapped in a ref."
  [r & body]
  `(let [r# ~r]
     (binding [*registry*
               (if-not (= clojure.lang.Ref (type r#))
                 (ref (if (isa? (type r#) clojure.lang.IDeref) @r# r#)) r#)]
       (do ~@body))))

(defn lookup
  "Returns the type for given id from the `*registry*`."
  [id] (get @*registry* id))

(declare make-struct)

(defn register!
  "Registers a pre-build struct or type with the given `id` or builds and
  registers a number of structs for the given specs.
  Throws IllegalArgumentException for duplicate IDs.
  Returns the last type registered.
  
  Each spec is a vector of: `[typename & fields]`

      ; declare a primitive OpenCL type
      (register! :float3 (primitive-vec* \"float3\" (lookup :float) 3))
      ; create a struct of a 3-element float3 array & single uint
      (register! :Face (make-struct 'Face [:verts :float3 3] [:color :uint]))

      ; register multiple types at once using make-struct
      (register!
        [[:Vec3 [:x :float] [:y :float] [:z :float]]
         [:Camera [:pos :Vec3] [:target :Vec3] [:up :Vec3]]])"
  ([specs]
    (doseq [[id & fields] specs]
      (register! id (apply make-struct id fields)))
    (lookup (first (last specs))))
  ([id type]
    (if (id @*registry*)
      (throw (IllegalArgumentException. (str "duplicate type " id)))
      (dosync (alter *registry* assoc id type)))
    type))

(defn deep-merge-with
  "Like merge-with, but merges maps recursively, applying the given fn
  only when there's a non-map at a particular level.

      (deep-merge-with +
        {:a {:b {:c 1 :d {:x 1 :y 2}} :e 3} :f 4}
        {:a {:b {:c 2 :d {:z 9} :z 3} :e 100}})
      => {:a {:b {:z 3, :c 3, :d {:z 9, :x 1, :y 2}}, :e 103}, :f 4}"
  ^{:author "Chris Houser"}
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
    (fn [m [k v]]
      (if-let [ks (k (struct-spec struct))]
        (assoc m k 
          (cond
            (map? v)
              (deep-select-keys v ks)
            (and (sequential? v) (not (primitive? ks)))
              (map #(deep-select-keys % (element-type ks)) v)
            :default v))
        m))
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
  (let [tname (name tname)
        spec (build-spec* fields)
        tpl (build-template* spec)
        codec (build-codec* spec)
        frame (gc/compile-frame (apply gc/ordered-map codec))
        size (gc/sizeof frame)
        spec-map (reduce (fn [acc {:keys[id element]}] (assoc acc id element)) {} spec)]
    (reify
      StructElement
      (cdeclare [this id] (str tname " " id ";"))
      (cname [this] tname)
      (compile-type [this] frame)
      (element-type [this] this)
      (length [this] 0)
      (primitive? [this] false)
      (sizeof [this] size)
      (template [this] (template this false))
      (template [this all?]
        (if all? tpl (deep-select-keys tpl this)))
      Struct
      (encode ^ByteBuffer [this m]
        (first (gio/encode frame (deep-merge-with merge-with-template tpl m))))
      (decode [this bytes]
        (deep-select-keys (gio/decode frame bytes) this))
      (dependencies [this]
        (dependencies this (dep/graph)))
      (dependencies [this g]
        (let [deps (->> (vals spec-map)
                        (filter (complement primitive?))
                        (map #(if (pos? (length %)) (element-type %) %))
                        (into #{}))
              g (reduce (fn [g t] (dependencies t g)) g deps)]
          (reduce (fn [g t] (dep/depend g this t)) g deps)))
      (gen-source* [this]
        (apply str
          "typedef struct {\n"
          (concat
            (mapcat (fn [{id :id e :element}]
                      ["  " (cdeclare e (name id)) "\n"]) spec)
            ["} " tname ";\n\n"])))
      (gen-source [this]
        (let [g (dependencies this)
              deps (dep/transitive-dependencies g this)
              sorted-deps (vec (dep/topo-sort g (vec (into #{this} deps))))]
          (apply str (header) (map gen-source* sorted-deps))))
      (struct-spec [this] spec-map))))
