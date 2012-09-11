(ns structgen.core-test
  (:use clojure.test structgen.core))

(dosync (ref-set *registry* @(make-registry)))

(register!
  [[:Vec2 [:x :float] [:y :float]]
   [:Vec3 [:x :float] [:y :float] [:z :float]]
   [:Camera [:pos :Vec3] [:target :Vec3] [:up :Vec3]]
   [:RSpec [:cam :Camera] [:light :Vec3 8] [:mouse :Vec2] [:col :float 4]]])

(println (gen-source (:RSpec @*registry*)))

(deftest test-encode-decode
  (let [buf (encode (:RSpec @*registry*)
                    {:cam {:pos {:z 400} :up {:y 1}}
                     :mouse {:x 200 :y 200}
                     :col [1 1 0 0.5]})]
    (decode (:RSpec @*registry*) buf)))


