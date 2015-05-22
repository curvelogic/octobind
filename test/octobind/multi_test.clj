(ns octobind.multi-test
  (:use [clojure.test :refer (deftest is)]
        [octobind.multi :as multi]
        [com.stuartsierra.component :as component]))

;; Some test dependencies

(defrecord Dep [])
(defrecord One [handler])
(defrecord Two [handler])
(defrecord Three [handler])

;; Simple component that knows whether it is started or not
(defrecord Dependency [started]
  component/Lifecycle
  (start [self] (println "starting") (assoc self :started true))
  (stop [self] (println "stopping") (assoc self :started false)))

(defn dependency [] (Dependency. false))

(defrecord Dependent [handlers])
(defn dependent [] (Dependent. nil))

(deftest bound-deps-are-available
  (let [sys (component/system-map
             :one 1
             :two 2
             :three 3
             ;; :collect is a multibinding that is passed 1 2 3
             :collect (multi/bind :one :two :three)
             
             ;; use the :collect multibind for the :handlers dependency
             :app (component/using (dependent)
                                   {:handlers :collect}))]

    (is (= (get-in sys [:app :handlers]) nil))

    (is (= (type (:collect sys)) octobind.multi.Multibind))
    
    (is (= (type (get-in (component/start sys) [:app :handlers])) octobind.multi.Multibind))

    ;; use invocation for access to the dependencies at use-time
    (is (= ((get-in (component/start sys) [:app :handlers])) [1 2 3]))))


(deftest bound-deps-are-started-and-stopped
  (let [sys (component/system-map
             :a (dependency)
             :b (dependency)
             :c (dependency)
             :all (multi/bind :a :b :c)
             :app (component/using (dependent) {:handlers :all}))]
    
    (is (not (:started (:app sys))))

    (let [started (component/start sys)
          handlers (get-in started [:app :handlers])
          deps (handlers)]
      (is (every? :started deps))
      (println "Stopping")
      (println "(:c cmp)" (:c started))
      
      (let [stopped (component/stop started)
            deps ((get-in stopped [:app :handlers]))]
        (println deps)
        (is (not (some :started deps)))))))


