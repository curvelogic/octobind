(ns octobind.multi
  (:require [com.stuartsierra.component :as component]
            [clojure.tools.logging :refer (info)])
  (:import [clojure.lang MapEntry]))

;; Multibind collects together the dependencies specified in deps
(deftype Multibind [f deps binds metadata]
  
  component/Lifecycle
  (start [self] self)
  (stop [self] self)

  ;; Access to the collected dependencies from the dependent object is
  ;; via invocation.
  clojure.lang.IFn
  (invoke [self] (map f binds))

  ;; Metadata is required for component dependencies
  clojure.lang.IObj
  (meta [self] metadata)
  (withMeta [self m] (Multibind. f deps binds m))
    
  ;; Component requires that we resemble a map for associng
  ;; dependencies 
  clojure.lang.Associative
  (containsKey [self k]
    (or (contains? #{:f :deps :binds} k)
        (and (number? k)
             (< k (count deps)))))

  (entryAt [self k]
    (let [entry (fn [k v] (MapEntry. k v))]
      (case k
        :f #spy/d (entry k f)
        :deps #spy/d (entry k deps)
        :binds #spy/d (entry k binds)
        (entry k (get binds k)))))
  
  (assoc [self k v]
    (if-let [index (deps k)]
      (Multibind. f deps (assoc binds index v) metadata)
      #spy/d (case k
        :f (Multibind. v deps binds metadata)
        :deps (Multibind. f v binds metadata)
        :binds (Multibind. f deps v metadata)
        self)))

  (valAt [self k] (.entryAt self k))

  (valAt [self k default] (or (.entryAt self k) default)))

(defn bind
  "Creates a multibinding which will gather the dependencies named by
  keyword in `deps`. If `f` is supplied it is used to extract from or
  transform the dependencies when component/start is called."
  ([f deps]
   (let [deps-map (into {} (map-indexed (fn [a b] [b a]) deps))]
     (component/using
      (Multibind. f deps-map (into [] (repeat (count deps) nil)) {})
      deps)))
  ([deps]
   (bind identity deps)))
