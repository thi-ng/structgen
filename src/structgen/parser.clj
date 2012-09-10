(ns structgen.parser
  (:require
  	[net.cgrand.parsley :as p]
  	[clojure.string :as string]))

(def ^:dynamic *defines* (atom {}))

(defmulti transform-node (fn [t c] t))

(defmethod transform-node :sym [t c]
	{:tag t :content (get @*defines* (first c) (first c))})

(defmethod transform-node :num [t c]
	{:tag t :content (first c)})

(defmethod transform-node :array [t c]
	{:tag t :content (second c)})

(defmethod transform-node :decl [t c]
	(let [[type id len] (map #(-> % :content) c)
  			 len (if len (Integer/parseInt (:content len)) 0)]
  	{:tag t :content [(keyword id) (keyword type) len]}))

(defmethod transform-node :typedef [t c]
  {:tag t
   :content {
     :declares (map :content (filter #(and (map? %) (= :decl (:tag %))) c))
  	 :id (-> (filter #(and (map? %) (= :sym (:tag %))) c) first :content keyword)}})

(defmethod transform-node :define [t c]
  (let [k (-> c (nth 1) :content)
  			v (-> c (nth 2) :content first :content)]
  	(swap! *defines* assoc k v)
  	{:tag t :content {:id k :value v}}))

(defmethod transform-node :default [t c]
	{:tag t :content c})

(def parse-typedefs
  (p/parser
  	{:main :expr*
  	:space :ws?
  	:make-node #(transform-node %1 (filter (complement nil?) %2))
    :make-leaf (fn[x] (when-not (string/blank? x) x))}
	:ws- #"\s+"
	:expr- #{:define :pragma :typedef}
	:sym #"[a-zA-Z_][\w]*"
	:num #"\d+"
	:atom #{:sym :num}
	:array ["[" :num "]"]
	:decl [:sym :sym :array? ";"]
	:typedef ["typedef struct {" :decl* "}" :sym ";"]
	:define ["#define" :sym :atom]
	:pragma ["#pragma" :sym :atom]))

(defn parsetree->specs
  [tree]
  (map
    #(let [{:keys [id declares]} (:content %)] (vec (cons id declares)))
    (filter #(= :typedef (:tag %)) (:content tree))))
