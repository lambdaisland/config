(ns lambdaisland.config.munge-test
  (:require
   [clojure.test :refer [deftest is]]
   [lambdaisland.config.munge :as munge]))

(deftest env-case-test
  (is (= "HELLO_WORLD" (munge/env-case "hello-world")))
  (is (= "FOO_BAR" (munge/env-case "foo/bar")))
  (is (= "MIXED_CASE" (munge/env-case "mixed-Case")))
  (is (= "ALREADY_UNDERSCORED" (munge/env-case "already_underscored"))))

(deftest key->env-var-test
  (is (= "MY_APP__SERVICE__API_KEY" (munge/key->env-var "my-app" :service/api-key)))
  (is (= "DATABASE_URL" (munge/key->env-var nil :database-url)))
  (is (= "APP__RAW_STRING" (munge/key->env-var "app" "RAW_STRING")))
  (is (= "APP__UNQUALIFIED" (munge/key->env-var "app" :unqualified)))
  (is (= "SIMPLE" (munge/key->env-var nil :simple))))

(deftest file-name-test
  (is (= "my-app-service-api-key" (munge/file-name "my-app" :service/api-key)))
  (is (= "app-simple" (munge/file-name "app" :simple)))
  (is (= "app-specialchars" (munge/file-name "app" :*special<chars>!))))
