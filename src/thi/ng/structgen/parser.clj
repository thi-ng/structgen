(ns thi.ng.structgen.parser
  "Basic C struct/typedef parser functions."
  ^{:author "Karsten Schmidt"}
  (:require
  	[net.cgrand.parsley :as p]
  	[clojure.string :as str]))

(def c-ws #"\s+")
(def c-symbol #"[a-zA-Z_][\w]*")
(def c-float #"[\-\+]?\d+[Ee\.]?[\dEe\-\+]*")

(def c-define
  (re-pattern
    (str "#define (" c-symbol ") ((" c-symbol ")|(" c-float "))" c-ws)))

(def ignore-terminals #"[\s\[\]\;]+")

(defmulti transform-node (fn [t c] t))

(defmethod transform-node :sym [t c]
  {:tag t :content (first c)})

(defmethod transform-node :num [t c]
  {:tag t :content (first c)})

(defmethod transform-node :array [t c]
  {:tag t :content (:content (first c))})

(defmethod transform-node :decl [t c]
  (let [[type id len] (map #(-> % :content) c)
  	len (if len (Integer/parseInt len))]
    {:tag t :content [(keyword id) (keyword type) len]}))

(defmethod transform-node :typedef [t c]
  {:tag t
   :content {
     :declares (map :content (filter #(and (map? %) (= :decl (:tag %))) c))
     :id (-> (filter #(and (map? %) (= :sym (:tag %))) c) first :content keyword)}})

(defmethod transform-node :default [t c]
  {:tag t :content c})

(defn preprocess
  ([src & {:as userdefs}]
    (let [defines (re-seq c-define src)]
      (reduce
        (fn[s [o d r]]
          (-> s
            (str/replace o "")
            (str/replace d (str (get userdefs (keyword d) r)))))
        src defines))))

(def parser
  (p/parser
    {:main :expr*
     :space :ws?
     :make-node #(transform-node %1 (filter (complement nil?) %2))
     :make-leaf (fn[x] (when-not (re-matches ignore-terminals x) x))
     }
    :expr- #{:pragma :typedef}
    :ws- c-ws
    :sym c-symbol
    :num c-float
    :atom #{:sym :num}
    :a-start- \[
    :a-end- \]
    :term- \;
    :array [:a-start :num :a-end]
    :decl [:sym :sym :array? :term]
    :typedef ["typedef struct {" :decl* "}" :sym :term]
    :pragma ["#pragma" :sym :atom]))

(defn tree->specs
  [tree]
  (map
    #(let [{:keys [id declares]} (:content %)] (vec (cons id declares)))
    (filter #(= :typedef (:tag %)) (:content tree))))

(defn parse
  [src & userdefs]
  (parser (apply preprocess src userdefs)))

(defn parse-specs
  [src & userdefs]
  (-> (apply preprocess src userdefs) (parser) (tree->specs)))
