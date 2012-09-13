(ns structgen.align
  (:use structgen.protocols))

(defn ceil-multiple-of
  "Rounds up `b` to a multiple of `a`."
  [a b]
  (let [r (rem b a)] (if (zero? r) b (- (+ b a) r))))

(defn primvec3?
  [x]
  (let [p (prim-type x)]
    (and (primitive? p) (= 3 (length p)))))

(defn opencl-alignment
  [e offset]
  (cond
    (primvec3? e)
      (do (prn "v3") (ceil-multiple-of (* 4 (sizeof (element-type (prim-type e)))) offset))
    (primitive? (prim-type e))
      (do (prn "prim") (ceil-multiple-of (sizeof (prim-type e)) offset))
    :default
      (do (prn "default") (ceil-multiple-of (sizeof (prim-type e)) offset))))

(defn packed-alignment
  [e offset] offset)