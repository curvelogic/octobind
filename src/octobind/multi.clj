(ns octobind.multi
  (:require [com.stuartsierra.component :as component]
            [clojure.tools.logging :refer (info)])
  (:import [clojure.lang MapEntry]))

;; Multibind collects together the dependencies specified in deps
(deftype Multibind [deps binds metadata]
  
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
  (withMeta [self m] (Multibind. deps binds m))
    
  ;; Component requires that we resemble a map for associng
  ;; dependencies 
  clojure.lang.Associative
  (containsKey [self k]
    (or (contains? #{:deps :binds} k)
        (and (number? k)
             (< k (count deps)))))

  (entryAt [self k]
    (let [entry (fn [k v] (MapEntry. k v))]
      (case k
        :deps (entry k deps)
        :binds (entry k binds)
        (entry k (get binds k)))))
  
  (assoc [self k v]
    (if-let [index (deps k)]
      (Multibind. deps (assoc binds index v) metadata)
      (case k
        :deps (Multibind. v binds metadata)
        :binds (Multibind. deps v metadata)
        self)))

  (valAt [self k] (.entryAt self k))

  (valAt [self k default] (or (.entryAt self k) default)))

(defn bind
  "Creates a multibinding which will gather the dependencies named by
  keyword in `deps`. If `f` is supplied it is used to extract from or
  transform the dependencies when component/start is called."
  [& deps]
  (let [deps-map (into {} (map-indexed (fn [a b] [b a]) deps))]
    (component/using
     (Multibind. deps-map (into [] (repeat (count deps) nil)) {})
     (vec deps))))
