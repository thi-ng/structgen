(ns structgen.parser-test
  (:use
    clojure.test
    [clojure.pprint :only [pprint]] 
    [structgen core parser]))

(dosync (ref-set *registry* @(make-registry)))

(def t
  (parse-typedefs
"#define _FLOAT_ double

typedef struct {
  _FLOAT_ x;
  _FLOAT_ y;
  _FLOAT_ z;
} Vec3;

typedef struct {
  Vec3 pos;
  Vec3 target;
  Vec3 up;
} Camera;

typedef struct {
  _FLOAT_ x;
  _FLOAT_ y;
} Vec2;

typedef struct {
  Camera cam;
  Vec3 lights[8];
  Vec2 mouse;
  _FLOAT_ col[4];
} RSpec;"))

(pprint t)

(register! (parsetree->specs t))

(println (gen-source (registed-type :RSpec)))

(decode (registed-type :RSpec)
        (encode (:RSpec @*registry*)
                {:cam {:pos {:x 400 :y 100 :z -1000} :up {:y 1}}
                 :lights [{:x 1 :y 2 :z 3} {:x 100 :y 1000 :z 10000}]
                 :mouse {:x 200 :y 600}
                 :col [0.8 0.9 1.0 0.5]}))

