# structgen

A Clojure library for seamless working with native C structs & structured byte buffers.

Structgen has the following features so far:

* parsing of typedefs from C source files
* dependency graph for nested structs
* C source code generation for registered types (incl. automatic dependencies)
* representation of C structs as standard Clojure data structures (maps & vectors)
* customizable memory alignment logic for struct fields (defaults to OpenCL alignment, inserts filler bytes where necessary)
* extensible type registry, pre-configured with common C & OpenCL primitives

Currently **not** supported:

* No self-recursive structs
* No pointer struct fields

Structgen is making use of ztellman's gloss library to encode/decode structs into/from byte buffers.

## Leiningen coordinates

    [com.postspectacular/structgen "0.1.1"]

## Usage

    (use '[structgen core parser])
    
    (reset-registry!)
    (register!
      (parse-specs
        "typedef struct {
           float x;
           float y;
           float z;
         } Vec3;
    
         typedef struct {
           Vec3 eye;
           Vec3 target;
           Vec3 up;
         } Cam;
    
         typedef struct {
           Vec3 a;
           Vec3 b;
           Vec3 c;
           Vec3 normal;
         } Face;
    
         typedef struct {
           int id;
           float transform[16];
           Face faces[1000];
         } Mesh;"))

Now we should have four new datatypes available in our registry and can start working with them:

    (template (lookup :Cam))
    ; {:eye {:x 0, :y 0, :z 0}, :target {:x 0, :y 0, :z 0}, :up {:x 0, :y 0, :z 0}}

Templates are used during encoding & decoding to ensure a valid data structure even if user data given is incomplete as in the following example where we've omitted the :target vector and only defined the :y key of the :up vector field. User data is always merged with the template during encoding:
    
    (def buf (encode (lookup :Cam) {:eye {:x 100 :y 200 :z 300} :up {:y 1}}))
    ; #'user/buf

Our camera definition is now encoded in a NIO ByteBuffer. Decoding this buffer with the correct struct type results again in a standard Clojure map:

    (decode (lookup :Cam) buf)
    ; {:up {:sg_align__6977 [0 0 0 0], :z 0.0, :y 1.0, :x 0.0},
    ;  :target {:sg_align__6977 [0 0 0 0], :z 0.0, :y 0.0, :x 0.0},
    ;  :eye {:sg_align__6977 [0 0 0 0], :z 300.0, :y 200.0, :x 100.0}}

Keys with the `:sg_align__` prefix in the map above identify the automatically generated filler blocks used to achieve correct memory alignment. These can also be suppressed by passing an additional `true` arg to the `decode` fn (though filtering them out can be much slower):

    (decode (lookup :Cam) buf true)
    ; {:eye {:x 100.0, :y 200.0, :z 300.0}, :target {:x 0.0, :y 0.0, :z 0.0}, :up {:x 0.0, :y 1.0, :z 0.0}}

To get a better idea about the internals of our data structures (e.g. memory requirements, field types), we can also inspect them:

    (->> :Mesh
      lookup
      struct-spec
      (map (fn [[k v]] [k {:size (sizeof v) :cname (cname v)}]))
      (into {}))
    ; {:faces {:size 64000, :cname "Face"}, :transform {:size 64, :cname "float"}, :id {:size 4, :cname "int"}}

Last but not least, we can also define new structs directly in Clojure (no need for C source) using simple type specs

    (register! :ColFace (make-struct 'ColFace [:verts :float3 3] [:color :uint]))
    ; or
    (register! [[:ColFace [:verts :float3 3] [:color :uint]]])

If we then later need to use this struct from C, we can generate the necessary header files like this:

    (spit "mesh.h" (gen-source (lookup :Mesh)))

More details can be found in the Marginalia docs: <url>

## License

Copyright © 2012 Karsten Schmidt / PostSpectacular Ltd.

Distributed under the Eclipse Public License, the same as Clojure.
