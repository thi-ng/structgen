(ns structgen.parser-test
  (:use
    clojure.test
    [clojure.pprint :only [pprint]] 
    [structgen core parser]))

;(dosync (ref-set *registry* @(make-registry)))
(reset-registry!)

(with-registry (make-registry)

  (def src
"#define _FLOAT_ double
#define NUM_LIGHTS 8

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
  Vec3 lights[NUM_LIGHTS];
  Vec2 mouse;
  _FLOAT_ col[4];
} RSpec;")

(pprint (parse src))

(def rspec (register! (parse-specs src)))

(println (gen-source rspec))

(prn (decode
  rspec
  (encode
    rspec
    {:cam {:pos {:x 400 :y 100 :z -1000} :up {:y 1}}
     :lights [{:x 1 :y 2 :z 3} {:x 100 :y 1000 :z 10000}]
     :mouse {:x 200 :y 600}
     :col [0.8 0.9 1.0 0.5]})))

(prn "bound" *registry*)
)

(prn "unbound" *registry*)