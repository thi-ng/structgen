(ns thi.ng.structgen.test.core
  (:require
   [thi.ng.structgen.core :as sg]
   [clojure.test :refer :all]))

(sg/reset-registry!)

(def rspec
  (sg/register!
   [[:Vec2   [:x :float] [:y :float]]
    [:Vec3   [:x :float] [:y :float] [:z :float]]
    [:Camera [:pos :Vec3] [:target :Vec3] [:up :Vec3]]
    [:RSpec  [:cam :Camera] [:light :Vec3 8] [:mouse :Vec2] [:col :float 4]]]))

(println (sg/generate-source rspec))

(deftest test-encode-decode
  (let [data {:cam   {:pos {:z 400} :up {:y 1}}
              :mouse {:x 200 :y 200}
              :light [{:x 1 :y 2 :z 3}]
              :col   [1 1 0 0.5]}
        buf (sg/encode rspec data)]
    (sg/deep-merge-with
     sg/merge-with-template
     (sg/template rspec)
     (sg/decode rspec buf))))
