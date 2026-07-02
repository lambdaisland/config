(ns lambdaisland.config-test
  (:require
   [clojure.test :refer [deftest is]]
   [clojure.core :as c]
   [lambdaisland.config :as config]))

(deftest get-and-source-test
  (let [p   (config/->MapProvider {:host "localhost" :port 5432} "literal map")
        cfg (config/new-config :dev [p])]
    (is (= "localhost" (config/get cfg :host)))
    (is (= ":host literal map" (config/source cfg :host)))
    (is (= nil (config/get cfg :missing)))
    (is (= nil (config/source cfg :missing)))))

(deftest entries-values-sources-test
  (let [p   (config/->MapProvider {:a 1 :b 2} "test")
        cfg (config/new-config :dev [p])]
    (config/get cfg :a)
    (config/get cfg :b)
    (is (= {:a {:val 1 :source ":a test" :provider p}
            :b {:val 2 :source ":b test" :provider p}}
           (config/entries cfg)))
    (is (= {:a 1 :b 2} (config/values cfg)))
    (is (= {:a ":a test" :b ":b test"} (config/sources cfg)))))

(deftest provider-order-test
  (let [first-p  (config/->MapProvider {:key :first} "first")
        second-p (config/->MapProvider {:key :second} "second")
        cfg      (config/new-config :dev [first-p second-p])]
    (is (= :first (config/get cfg :key)))
    (is (= ":key first" (config/source cfg :key)))))

(deftest reload-test
  (let [a   (atom {:key :old})
        p   (config/->DerefMapProvider a "test")
        cfg (config/new-config :dev [p])]
    (is (= :old (config/get cfg :key)))
    (reset! a {:key :new})
    (config/reload! cfg)
    (is (= :new (config/get cfg :key)))))
