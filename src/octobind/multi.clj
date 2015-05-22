(ns octobind.multi
  (:require [com.stuartsierra.component :as component])
  (:import [clojure.lang MapEntry]))

;; Multibind collects together the dependencies specified in deps
(deftype Multibind [dep-index-map binds metadata]
  
  component/Lifecycle
  (start [self] self)
  (stop [self] self)

  ;; Access to the collected dependencies from the dependent object is
  ;; via invocation.
  clojure.lang.IFn
  (invoke [self] binds)

  ;; Metadata is required for component dependencies
  clojure.lang.IObj
  (meta [self] metadata)
  (withMeta [self m] (Multibind. dep-index-map binds m))
    
  ;; Component requires that we resemble a map for associng
  ;; dependencies 
  clojure.lang.Associative
  (containsKey [self k]
    (or (contains? #{:dep-index-map :binds} k)
        (and (number? k)
             (< k (count dep-index-map)))))

  (entryAt [self k]
    (let [entry (fn [k v] (MapEntry. k v))]
      (case k
        :dep-index-map (entry k dep-index-map)
        :binds (entry k binds)
        (entry k (get binds k)))))
  
  (assoc [self k v]
    (println "setting" k "to" v)
    (if-let [index (dep-index-map k)]
      (Multibind. dep-index-map (assoc binds index v) metadata)
      (case k
        :dep-index-map (Multibind. v binds metadata)
        :binds (Multibind. dep-index-map v metadata)
        self)))

  (valAt [self k] (.entryAt self k))

  (valAt [self k default] (or (.entryAt self k) default)))

(defn bind
  "Creates a multibinding which will, on component/start, gather the
  dependencies named by keyword in `deps`."
  [& deps]
  (let [deps-map (into {} (map-indexed (fn [n d] [d n]) deps))]
    (component/using
     (Multibind. deps-map (into [] (repeat (count deps) nil)) {})
     (vec deps))))

