(ns octobind.multi-test
  (:use [clojure.test :refer (deftest is)]
        [octobind.multi :as multi]
        [com.stuartsierra.component :as component]))

(defrecord Dep [])
(defrecord One [handler])
(defrecord Two [handler])
(defrecord Three [handler])

(defrecord Dependency [started]
  component/Lifecycle
  (start [self] (assoc self :started true))
  (stop [self] (assoc self :started false)))
(defn dependency [] (Dependency. false))

(defrecord Dependent [handlers])
(defn dependent [] (Dependent. nil))

(deftest bound-deps-are-available
  (let [sys (component/system-map
             :one 1
             :two 2
             :three 3
             :collect (multi/bind :one :two :three)
             :app (component/using (dependent)
                                   {:handlers :collect}))]

    (is (= (get-in sys [:app :handlers]) nil))

    (is (= (type (:collect sys)) octobind.multi.Multibind))
    
    (is (= (type (get-in (component/start sys) [:app :handlers])) octobind.multi.Multibind))
    
    (is (= ((get-in (component/start sys) [:app :handlers])) [1 2 3]))))

(deftest bound-deps-are-started 
  (let [sys (component/system-map
             :a (dependency)
             :b (dependency)
             :c (dependency)
             :all (multi/bind :a :b :c)
             :app (component/using (dependent) {:handlers :all}))]
    
    (is (not (:started (:app sys))))
    (let [handlers (get-in (component/start sys) [:app :handlers])
          deps (handlers)]
      (is (every? :started deps)))))
