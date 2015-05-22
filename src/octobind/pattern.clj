(ns ^{:doc "Multi-bind via patterns"}
  octobind.pattern
  (:require [com.stuartsierra.component :as component]))

(defn pattern-binder?
  "Check whether a value (from a dependency map) is a pattern
  binding."
  [val]
  (= (:bind (meta val)) :pattern))

(defn find-pattern-dependencies
  "Retrieve all pattern bindings from an unaugmented system map."
  [system-map]
  (->> system-map
       vals
       (map component/dependencies)
       (mapcat vals)
       (filter pattern-binder?)))

(defn blank-map [m]
  (reduce #(assoc %1 %2 nil) {} (keys m)))

(defn intermediate-for-map [m]
  (component/using (blank-map m) m))

(defn intermediate-for-vector [v]
  (let [blank (vec (repeat (count v) nil))
        dependencies (into {} (map vector (range) v))]
    (component/using blank dependencies)))

(defn make-intermediate
  "Construct intermediate component with start implementation that
  resolves the patterns dependencies and transforms with fn"
  [pattern]
  (cond
    (vector? pattern) (intermediate-for-vector pattern)
    (map? pattern) (intermediate-for-map pattern)
    :else (throw (ex-info "Unsupported pattern." {:pattern pattern}))))

(defn replace-intermediate-meta [v]
  (let [original-meta (meta v)
        new-meta (clojure.walk/postwalk
                  (fn [x] (if (pattern-binder? x) (first x) x))
                  original-meta)]
    (with-meta v new-meta)))

(defn patternise
  "Process a system map to supplement it with entries for all the
  patterns and replace the pattern-dependencies with the keys."
  [system-map]
  (let [pattern-deps (find-pattern-dependencies system-map)
        augmented (reduce (fn [m [kw pattern]]
                            (assoc m kw (make-intermediate pattern)))
                          system-map
                          pattern-deps)]
    (->> (seq augmented)
         (map (fn [[k v]] [k (replace-intermediate-meta v)]))
         (apply concat)
         (apply component/system-map))))

(defmacro bind
  "Create a value which can act as a key in the augmented system map
  but also encode the operation required to resolve the dependency."
  [pattern]
  (with-meta [(keyword (gensym)) pattern] {:bind :pattern}))