(ns structgen.core-test
  (:use clojure.test structgen.core))

;(dosync (ref-set *registry* (make-registry)))
(reset-registry!)

(register!
  [[:Vec2 [:x :float] [:y :float]]
   [:Vec3 [:x :float] [:y :float] [:z :float]]
   [:Camera [:pos :Vec3] [:target :Vec3] [:up :Vec3]]
   [:RSpec [:cam :Camera] [:light :Vec3 8] [:mouse :Vec2] [:col :float 4]]])

(println (gen-source (registered-type :RSpec)))

(deftest test-encode-decode
  (let [buf (encode (registered-type :RSpec)
                    {:cam {:pos {:z 400} :up {:y 1}}
                     :mouse {:x 200 :y 200}
                     :col [1 1 0 0.5]})]
    (decode (registered-type :RSpec) buf)))


