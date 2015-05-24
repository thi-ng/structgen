(ns thi.ng.structgen.core
  "Utilities to seamlessly work with native C structs.
  Provides extensible struct generators and customizable
  alignment logic for converting between Clojure maps and
  byte buffers. Supports nested structs, arrays, custom C
  primitives (e.g as used by OpenCL) and endianess.
  Can generate C source from Clojure defined structs."
  ^{:author "Karsten Schmidt"}
  (:import
   [java.text SimpleDateFormat]
   [java.util Date])
  (:require
   [gloss.core :as gc]
   [gloss.io :as gio]
   [com.stuartsierra.dependency :as dep]))

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
  (clojure-type [this data]
    "Returns the element as Clojure data.")
  (length [this]
    "Returns the element's array length (zero for single values).")
  (primitive? [this]
    "Returns true if element is not a struct.")
  (prim-type [this])
  (sizeof [this]
    "Returns byte size of compiled element.")
  (template [this] [this all?]
    "Returns the element's data template: map for structs, vector for arrays"))

(defprotocol Struct
  "Defines functionality only available for composite types (structs)."
  (encode [this m]
    "Encodes the map `m` into a byte buffer. Missing values are replaced
    with values from the template.")
  (decode [this bytes] [this bytes filter?]
    "Decodes a byte buffer into map with structure as defined by the struct.
    If filter? is truthy the returned map will only contain data fields and
    has any alignment fields removed. Disabled by default.")
  (dependencies [this] [this g]
    "Returns the graph of all transitive dependencies for this struct.
    Use clojure.data.dependency for further analysis.")
  (struct-spec [this]
    "Returns a map of field names and StructElement or Struct implementations
    for the given struct.")
  (gen-source* [this]
    "Generates C source for the typedef of this struct.")
  (generate-source [this]
    "Builds a dependency graph and generates C source for the typedef of
    this struct and all required dependencies (in correct order)."))

;; # Helper fns

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
  "Recursively selects only keys from map `m` matching the given struct spec.
  This function is used internally by `decode` to exclude any alignment keys."
  [m struct]
  (reduce
   (fn [m [k v]]
     (if-let [ks (-> (struct-spec struct) k :element)]
       (assoc m k
              (cond
                (map? v)
                (deep-select-keys v ks)
                (and (sequential? v) (not (primitive? ks)))
                (map #(deep-select-keys % (element-type ks)) v)
                :default v))
       m))
   {} m))

(defn deep-decode
  "Recursively selects keys from map `m` and applies any configured
  field decoder fns."
  [m struct]
  (reduce
   (fn [m [k v]]
     (let [k-spec (k (struct-spec struct))
           ks (:element k-spec)]
       (if ks
         (assoc m k
                (if (map? v)
                  (deep-decode v ks)
                  (if (sequential? v)
                    (if (primitive? ks)
                      (clojure-type ks v)
                      (map #(deep-decode % (element-type ks)) v))
                    (if-let [d (:decode k-spec)]
                      (d v)
                      v))))
         (assoc m k v))))
   {} m))

(defn merge-with-template
  [tpl data]
  (cond
    (sequential? tpl)
    (let [ct (count tpl)
          cd (count data)]
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

(defn ceil-power2
  [x]
  (loop [pow2 1]
    (if (>= pow2 x)
      pow2
      (recur (* pow2 2)))))

(defn opencl-alignment
  [e offset stride]
  (ceil-multiple-of (min stride (ceil-power2 (sizeof (prim-type e)))) offset))

(defn packed-alignment
  [e offset stride] offset)

(def ^:dynamic *config*
  {:le true
   :align-fn opencl-alignment
   :stride 16})

(defmacro with-config
  [config & body]
  `(binding [*config* (merge *config* ~config)]
     (do ~@body)))

;; # C primitive datatype handling

(defn make-primitive
  "Returns a `StructElement` implementation for the given primitive type description:

  cname - C type name
  type - gloss codec type
  size - size in bytes"
  [cname type size]
  (let [cname (name cname)]
    (reify
      StructElement
      (cdeclare
        [this id] (str cname " " id ";"))
      (cname
        [this] cname)
      (compile-type
        [this]
        (gc/compile-frame
         (if (> size 1)
           (keyword (str (name type) (if (:le *config*) "-le" "-be")))
           type)))
      (element-type
        [this] this)
      (clojure-type
        [this data] data)
      (length
        [this] 0)
      (primitive?
        [this] true)
      (prim-type
        [this] this)
      (sizeof
        [this] size)
      (template
        [this] 0)
      (template
        [this _] 0))))

(defn make-primitive-vec
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

  (make-primitive-vec \"char2\" (lookup :char) 2)"
  [cname type len ctor]
  (reify
    StructElement
    (cdeclare
      [this id] (str cname " " id ";"))
    (cname
      [this] cname)
    (compile-type
      [this] (gc/compile-frame (repeat len (compile-type type))))
    (element-type
      [this] type)
    (clojure-type
      [this data] (ctor data))
    (length
      [this] len)
    (primitive?
      [this] true)
    (prim-type
      [this] this)
    (sizeof
      [this] (* len (sizeof type)))
    (template
      [this] (template this true))
    (template
      [this _] (vec (repeat len 0)))))

(defn make-element-array
  "Returns a `StructElement` implementation for the given element array description.
  Arrays can be created for any type (primitives, primitive vecs, structs). Currently
  only supports 1-dimensional arrays."
  ([e len ctor] (make-element-array e len (cname e) ctor))
  ([e len cname ctor]
   (reify
     StructElement
     (cdeclare
       [this id] (str cname " " id "[" len "];"))
     (cname
       [this] cname)
     (compile-type
       [this] (gc/compile-frame (repeat len (compile-type e))))
     (element-type
       [this] e)
     (clojure-type
       [this data]
       (if ctor
         (ctor data)
         (mapv #(clojure-type e %) data)))
     (length
       [this] len)
     (primitive?
       [this] (primitive? e))
     (prim-type
       [this] (prim-type e))
     (sizeof
       [this] (ceil-power2 (* len (sizeof e))))
     (template
       [this] (template this false))
     (template
       [this all?] (vec (repeat len (template e all?)))))))

;; # Type registration

(defn make-registry
  "Builds a vanilla type registry populated only with standard C
  and OpenCL primitive types."
  [& specs]
  (let [prims {:char   (make-primitive "char" :byte 1)
               :uchar  (make-primitive "unsigned char" :ubyte 1)
               :short  (make-primitive "short" :int16 2)
               :ushort (make-primitive "unsigned short" :uint16 2)
               :int    (make-primitive "int" :int32 4)
               :uint   (make-primitive "unsigned int" :uint32 4)
               :long   (make-primitive "long" :int64 8)
               :ulong  (make-primitive "unsigned long" :uint64 8)
               :float  (make-primitive "float" :float32 4)
               :double (make-primitive "double" :float64 8)}
        primvecs (for [t [:float :char :uchar :short :ushort
                          :int :uint :long :ulong]
                       n [2 3 4 8 16]]
                   (let [tname (str (name t) n)]
                     {(keyword tname) (make-primitive-vec tname (t prims) n vec)}))]
    (apply merge prims primvecs)))

(def ^:dynamic *registry* (atom (make-registry)))

(defn reset-registry!
  ([] (reset-registry! nil))
  ([r] (reset! *registry* (or r (make-registry)))))

(defmacro with-registry
  "Binds the type `*registry*` to the supplied map and executes body
  in a `do` form. The given registry value is automatically wrapped
  in or converted to an atom (if not already).
  If the custom registry is supposed to be used outside the macro
  scope at a later time, it **must** already be wrapped in an atom."
  [r & body]
  `(let [r# ~r]
     (binding [*registry*
               (if-not (= clojure.lang.Atom (type r#))
                 (atom (if (isa? (type r#) clojure.lang.IDeref) @r# r#)) r#)]
       (do ~@body))))

(defn lookup
  "Returns the type for given id from the `*registry*`."
  [id] (@*registry* id))

(declare make-struct)

(defn register!
  "Registers a pre-build struct or type with the given `id` or builds and
  registers a number of structs for the given specs. The default registry
  is initialized with all standard OpenCL primitive types.
  Shows a warning message when overriding existing type IDs.
  Returns the last type registered.

  Each spec is a vector of: `[typename & fields]`

  ; declare a primitive OpenCL type
  (register! :float3 (make-primitive-vec :float3 (lookup :float) 3))
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
   (when (id @*registry*)
     (prn (str "WARNING: overriding type spec " id)))
   (swap! *registry* assoc id type)
   type))

;; # Struct generation

(defn build-align-spec*
  [len]
  (into {}
        (when (pos? len)
          {:gid (keyword "thi.ng.structgen.core" (str (gensym "sg_align__")))
           :gcode (gc/compile-frame (repeat len :byte))
           :gdata (vec (repeat len 0))})))

(defn build-spec*
  [fields]
  (let[{:keys [align-fn stride]} *config*
       [spec offset total]
       (reduce
        (fn [[m offset total] [id t len decode-fn]]
          (if-let [e (get @*registry* t)]
            (let [[len decode-fn] (if (fn? len) [nil len] [len decode-fn])
                  e (if (and len (pos? len)) (make-element-array e len decode-fn) e)
                  s (sizeof e)
                  align (if (pos? offset) (align-fn e offset stride) 0)
                  gap (- align offset)]
              ;;(prn id :size s :offset (format "%03x" offset) :align (format "%03x" align) :gap gap :decode decode-fn)
              [(conj m (merge (build-align-spec* gap) {:id id :element e :decode decode-fn}))
               (+ align s)
               (+ align s)])
            (throw (IllegalArgumentException. (str "unknown type " t)))))
        [[] 0 0] fields)
       block-fill (- (ceil-multiple-of stride total) total)]
    (if (pos? block-fill)
      (concat spec [(build-align-spec* block-fill)])
      spec)))

(defn build-template*
  [spec]
  (apply merge
         (concat
          (map (fn [{:keys [id element]}] {id (template element true)}) (filter :id spec))
          (map (fn [{:keys [gid gdata]}] {gid gdata}) (filter :gid spec)))))

(defn build-codec*
  [spec]
  (reduce
   (fn [acc {:keys [id element gid gcode]}]
     (cond
       (and id gid) (conj acc gid gcode id (compile-type element))
       gid (conj acc gid gcode)
       :default (conj acc id (compile-type element))))
   [] spec))

(defn- header
  []
  (str
   "/* generated @ "
   (.format (SimpleDateFormat. "yyyy-MM-dd HH:mm:ss") (Date.))
   " by thi.ng/structgen */\n\n"))

(defn make-struct
  [tname & fields]
  (let [tname    (name tname)
        spec     (build-spec* fields)
        tpl      (build-template* spec)
        codec    (build-codec* spec)
        frame    (gc/compile-frame (apply gc/ordered-map codec))
        size     (gc/sizeof frame)
        spec     (filter (comp not nil? :id) spec)
        spec-map (into {} (map (fn [f] [(:id f) (dissoc f :id)])) spec)]
    (reify
      StructElement
      (cdeclare
        [this id] (str tname " " id ";"))
      (cname
        [this] tname)
      (compile-type
        [this] frame)
      (element-type
        [this] this)
      (length
        [this] 0)
      (primitive?
        [this] false)
      (prim-type
        [this] this)
      (sizeof
        [this] size)
      (template
        [this] (template this false))
      (template
        [this all?] (if all? tpl (deep-select-keys tpl this)))
      Struct
      (encode
        [this m]
        (first (gio/encode frame (deep-merge-with merge-with-template tpl m))))
      (decode
        [this bytes] (decode this bytes false))
      (decode
        [this bytes filter?]
        (deep-decode
         (if filter?
           (deep-select-keys (gio/decode frame bytes) this)
           (gio/decode frame bytes))
         this))
      (dependencies
        [this] (dependencies this (dep/graph)))
      (dependencies
        [this g]
        (let [deps (into #{}
                         (comp
                          (map :element)
                          (filter (complement primitive?))
                          (map #(if (pos? (length %)) (element-type %) %)))
                         (vals spec-map))
              g (reduce (fn [g t] (dependencies t g)) g deps)]
          (reduce (fn [g t] (dep/depend g this t)) g deps)))
      (gen-source*
        [this]
        (apply str
               "typedef struct {\n"
               (concat
                (mapcat
                 (fn [{id :id e :element}]
                   ["  " (cdeclare e (name id)) "\n"]) spec)
                ["} " tname ";\n\n"])))
      (generate-source
        [this]
        (let [g           (dependencies this)
              deps        (dep/transitive-dependencies g this)
              sorted-deps (vec (dep/topo-sort g))
              sorted-deps (if (seq sorted-deps) sorted-deps [this])]
          (apply str (header) (map gen-source* sorted-deps))))
      (struct-spec [this] spec-map))))
