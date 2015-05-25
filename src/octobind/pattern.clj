(ns ^{:doc "Supports binding of dependencies to vectors or maps of
  system-map entries."}
  octobind.pattern
  (:require [com.stuartsierra.component :as component]))

(defn- pattern-binder?
  "Check whether a value (from a dependency map) is a pattern
  binding."
  [val]
  (= (:bind (meta val)) :pattern))

(defn- map-vals [f m]
  (into {} (for [[k v] m] [k (f v)])))

(defn- tag-pattern-dependencies-of-value
  "Tag any collections in the dependencies metadata as pattern
  dependencies."
  [value]
  (->> (component/dependencies value)
       (map-vals (fn [v] (if (coll? v)
                          (with-meta
                            [(keyword (gensym)) v]
                            {:bind :pattern})
                          v)))
       (component/using value)))

(defn- tag-collections-as-pattern-dependencies
  "Tag any collection in the dependencies within a system map as
  pattern dependencies."
  [system-map]
  (->> system-map
       (map-vals tag-pattern-dependencies-of-value)))

(defn- find-pattern-dependencies
  "Retrieve all pattern bindings from an unaugmented system map."
  [system-map]
  (->> system-map
       vals
       (map component/dependencies)
       (mapcat vals)
       (filter pattern-binder?)))

(defn- blank-map [m]
  (reduce #(assoc %1 %2 nil) {} (keys m)))

(defn- intermediate-for-map [m]
  (component/using (blank-map m) m))

(defn- intermediate-for-vector [v]
  (let [blank (vec (repeat (count v) nil))
        dependencies (into {} (map vector (range) v))]
    (component/using blank dependencies)))

(defn- make-intermediate
  "Construct intermediate component with start implementation that
  resolves the patterns dependencies and transforms with fn"
  [pattern]
  (cond
    (vector? pattern) (intermediate-for-vector pattern)
    (map? pattern) (intermediate-for-map pattern)
    :else (throw (ex-info "Unsupported pattern" {:pattern pattern}))))

(defn- replace-intermediate-meta [v]
  (let [original-meta (meta v)
        new-meta (clojure.walk/postwalk
                  (fn [x] (if (pattern-binder? x) (first x) x))
                  original-meta)]
    (with-meta v new-meta)))

(defn with-collection-binds
  "Enhance a system map to support resolution of structured
  dependencies.

  For example:
  `(with-collection-binds
    (component/system-map
             :a (dependency)
             :b (dependency)
             :c (component/using
                 (dependent)
                 {:deps [:a :b]})
             :d (component/using
                 (dependent)
                 {:deps {:x :a :y :b}})))`

  Currently only a single level of structure (maps or vectors of
  keyword references to other dependencies) is supported.

  `with-collection-binds` supplements the system-map with new entries for
  intermediate dependencies for each of the patterns so be aware that
  iterating over the entries will reveals extraneous (gensym) keys."
  [system-map]
  (let [tagged-map (tag-collections-as-pattern-dependencies system-map)
        pattern-deps (find-pattern-dependencies tagged-map)
        augmented (reduce (fn [m [kw pattern]]
                            (assoc m kw (make-intermediate pattern)))
                          tagged-map
                          pattern-deps)]
    (->> (seq augmented)
         (map (fn [[k v]] [k (replace-intermediate-meta v)]))
         (apply concat)
         (apply component/system-map))))
