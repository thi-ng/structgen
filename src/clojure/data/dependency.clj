(ns clojure.data.dependency
  "Bidirectional graphs of dependencies and dependent objects."
  ^{:author "Stuart Sierra"
    :url "https://github.com/stuartsierra/data.dependency"}
  (:use [clojure.set :only (union)]))

(defprotocol DependencyGraph
  (immediate-dependencies [graph key]
    "Returns the set of immediate dependencies of key.")
  (immediate-dependents [graph key]
    "Returns the set of immediate dependents of key.")
  (transitive-dependencies [graph key]
    "Returns the set of all things which key depends on, directly or
    transitively.")
  (transitive-dependents [graph key]
    "Returns the set of all things which depend upon x, directly or
    transitively."))

(defprotocol DependencyGraphUpdate
  (depend [graph key dep]
    "Returns a new graph with a dependency from key to dep. Forbids
    circular dependencies.")
  (remove-all [graph key]
    "Returns a new dependency graph with all references to key removed.")
  (remove-key [graph key]
    "Removes the key from the dependency graph without removing it as a
    dependency of other keys."))

(defn- remove-from-map [amap x]
  (reduce (fn [m [k vs]]
	    (assoc m k (disj vs x)))
	  {} (dissoc amap x)))

(defn- transitive
  "Recursively expands the set of dependency relationships starting
  at (get m x)"
  [m x]
  (reduce (fn [s k]
	    (union s (transitive m k)))
	  (get m x) (get m x)))

(declare depends?)

(deftype MapDependencyGraph [dependencies dependents]
  DependencyGraph
  (immediate-dependencies [graph key]
    (get dependencies key))
  (immediate-dependents [graph key]
    (get dependents key))
  (transitive-dependencies [graph key]
    (transitive dependencies key))
  (transitive-dependents [graph key]
    (transitive dependents key))
  DependencyGraphUpdate
  (depend [graph key dep]
    (when (depends? graph dep key)
      (throw (Exception.
              (str "Circular dependency between "
                   (pr-str key) " and " (pr-str dep)))))
    (MapDependencyGraph.
     (update-in dependencies [key] union #{dep})
     (update-in dependents [dep] union #{key})))
  (remove-all [graph key]
    (MapDependencyGraph.
     (remove-from-map dependencies key)
     (remove-from-map dependents key)))
  (remove-key [graph key]
    (MapDependencyGraph.
     (dissoc dependencies key)
     dependents)))

(defn graph "Returns a new, empty, dependency graph." []
  (->MapDependencyGraph {} {}))

(defn depends?
  "True if x is directly or transitively dependent on y."
  [graph x y]
  (contains? (transitive-dependencies graph x) y))

(defn dependent?
  "True if y is a dependent of x."
  [graph x y]
  (contains? (transitive-dependents graph x) y))

(defn topo-comparator
  "Returns a comparator which produces a topographical sort based on
  the dependencies in graph."
  [graph]
  (comparator (partial dependent? graph)))

(defn topo-sort
  "Returns a topographically-sorted sequence of the items in coll
  using dependencies in graph."  
  [graph coll]
  (sort (topo-comparator graph) coll))

(defn topo-sort-by
  "Returns a topographically-sorted sequence of the items in coll by
  comparing (keyfn item) using dependencies in graph."
  [graph keyfn coll]
  (sort-by keyfn (topo-comparator graph) coll))

(comment
  ;; example usage: building a graph like:
  ;;
  ;;     :a -------- :b
  ;;       \          \
  ;;        \          \
  ;;         `-------- :c ---- :d
  ;;
  (def g (-> (graph)
             (depend :b :a)
             (depend :c :b)
             (depend :c :a)
             (depend :d :c)))

  (transitive-dependencies g :d)
  ;;=> #{:a :c :b}

  (topo-sort g [:d :a :c :b])
  ;;=> (:a :b :c :d)
  )
