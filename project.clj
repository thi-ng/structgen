(defproject thi.ng/structgen "0.2.1"
  :description  "Interop library for working with native C structs and binary formats in general."
  :url          "http://thi.ng/structgen"
  :license      {:name "Apache Software License 2.0"
                 :url "http://www.apache.org/licenses/LICENSE-2.0"
                 :distribution :repo}
  :scm          {:name "git"
                 :url "git@github.com:thi-ng/structgen.git"}

  :dependencies [[org.clojure/clojure "1.7.0-beta3"]
                 [com.stuartsierra/dependency "0.1.1"]
                 [net.cgrand/parsley "0.9.3" :exclusions [org.clojure/clojure]]
                 [gloss "0.2.5"]]

  :profiles     {:dev {:dependencies [[criterium "0.4.3"]]
                       :global-vars {*warn-on-reflection* true}
                       :jvm-opts ^:replace []
                       :aliases {"cleantest" ["do" "clean," "test"]}}}
  
  :pom-addition [:developers [:developer
                              [:name "Karsten Schmidt"]
                              [:url "http://postspectacular.com"]
                              [:timezone "0"]]])
