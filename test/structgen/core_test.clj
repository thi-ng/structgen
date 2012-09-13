(ns structgen.core-test
  (:use
    clojure.test
    structgen.core))

(reset-registry!)

(def rspec
  (register!
    [[:Vec2 [:x :float] [:y :float]]
     [:Vec3 [:x :float] [:y :float] [:z :float]]
     [:Camera [:pos :Vec3] [:target :Vec3] [:up :Vec3]]
     [:RSpec [:cam :Camera] [:light :Vec3 8] [:mouse :Vec2] [:col :float 4]]]))

(println (gen-source rspec))

(deftest test-encode-decode
  (let [data {:cam {:pos {:z 400} :up {:y 1}}
              :mouse {:x 200 :y 200}
              :light [{:x 1 :y 2 :z 3}]
              :col [1 1 0 0.5]}
        buf (encode rspec data)]
    (is = (deep-merge-with merge-with-template
            (template rspec)
            (decode rspec buf)))))
