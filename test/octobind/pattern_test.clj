(ns octobind.pattern-test
  (:require [com.stuartsierra.component :as component]
            [octobind.pattern :as pattern]
            [clojure.core.match :refer (match)]
            [clojure.test :refer (deftest is)]))

(deftest pattern-bind
  
  (is (second (pattern/bind [:a :b])) [:a :b])

  (is (second (pattern/bind {:x :a :y :z})) {:x :a :y :z} ))

(defrecord Dependency [])
(defn dependency [] (->Dependency))

(defrecord Dependent [deps])
(defn dependent [] (->Dependent nil))

(deftest patternise
  (let [sys (component/system-map
             :a (dependency)
             :b (dependency)
             :c (component/using
                 (dependent)
                 {:deps (pattern/bind [:a :b])})
             :d (component/using
                 (dependent)
                 {:deps (pattern/bind {:x :a :y :b})}))]

    ;; check we only have the expected keys to start with
    (is (= (keys sys) [:a :b :c :d]))

    ;; check we identify only the pattern binds (by metadata match)
    (is (= (count (map first (pattern/find-pattern-dependencies sys))) 2))

    ;; check the values of the pattern binds are as expected
    (let [dependencies (:com.stuartsierra.component/dependencies (meta (:c sys)))]
      (is (some #{:deps} (keys dependencies)))
      (is (= (second (:deps dependencies))
             [:a :b])))

    ;; check that the intermediate dependencies have been added to the
    ;; augmented map
    (let [kw (-> sys :c meta :com.stuartsierra.component/dependencies :deps first)
          augmented (pattern/patternise sys)]
      (is (some #{kw} (keys augmented)))

      (let [started (component/start augmented)]

        (println started)
        (is (not (nil? (kw started))))
        (is (= (:deps (:d started)) {:x (:a started) :y (:b started)}))
        (is (= (:deps (:c started)) [(:a started) (:b started)]))))))
