(ns octobind.pattern-test
  (:require [com.stuartsierra.component :as component]
            [octobind.pattern :as pattern]
            [clojure.core.match :refer (match)]
            [clojure.test :refer (deftest is are)]))

(defrecord Dependency [])
(defn dependency [] (->Dependency))

(defrecord Dependent [deps])
(defn dependent [] (->Dependent nil))

(deftest pattern-dependencies
  (let [sys (component/system-map
             :a (dependency)
             :b (dependency)
             :c (component/using
                 (dependent)
                 {:deps [:a :b]})
             :d (component/using
                 (dependent)
                 {:deps {:x :a :y :b}}))]

    (let [augmented (pattern/patternise sys)]

      ;; the augmented system map will have extra intermediate
      ;; dependencies
      (is (> (count augmented) 4))

      ;; ...but the explicit values won't have changed (apart from
      ;; metadata)
      (are [kw] (= (kw augmented) (kw sys))
        :a :b :c :d)

      ;; Once started, all pattern dependencies are correctly resolved
      (let [started (component/start augmented)]
        (is (= (:deps (:d started)) {:x (:a started) :y (:b started)}))
        (is (= (:deps (:c started)) [(:a started) (:b started)]))))))
