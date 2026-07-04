(ns lambdaisland.config-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is use-fixtures]]
   [lambdaisland.config :as config]
   [lambdaisland.config.test-helpers :as helpers]))

(defn each-fixture [f]
  (helpers/setenv {"MYAPP__ENV" "" "CI" "false" "MYAPP__DB__HOST" "env-host"})
  (f))

(use-fixtures :each each-fixture)

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

(deftest env-key-from-explicit
  (is (= :prod (config/env-key {:env :prod}))))

(deftest env-key-from-env-var
  (helpers/setenv {"MYAPP__ENV" "prod"})
  (is (= :prod (config/env-key {:prefix "myapp"}))))

(deftest env-key-ci
  (helpers/setenv {"CI" "true"})
  (is (= :test (config/env-key {}))))

(deftest env-key-default
  (helpers/setenv {"CI" "false"})
  (is (= :dev (config/env-key {}))))

(deftest env-provider-test
  (helpers/setenv {"MYAPP__DB__HOST" "env-db-host"})
  (let [p (config/->EnvProvider "myapp")
        cfg (config/new-config :dev [p])]
    (is (= "env-db-host" (config/get cfg :db/host)))
    (is (str/includes? (config/source cfg :db/host) "MYAPP__DB__HOST"))))
