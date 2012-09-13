(ns structgen.protocols)

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
