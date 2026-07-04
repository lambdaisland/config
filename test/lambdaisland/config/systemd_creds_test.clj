(ns lambdaisland.config.systemd-creds-test
  (:require
   [clojure.java.io :as io]
   [clojure.java.shell :as sh]
   [clojure.test :refer :all]
   [lambdaisland.config :as config]
   [lambdaisland.config.systemd-creds :as systemd-creds]
   [lambdaisland.config.test-helpers :as helpers]))

(def tmp-path "/tmp/li-config-test-tmp")

(deftest systemd-creds-test
  (sh/sh "rm" "-rf" tmp-path)
  (.mkdirs (io/file tmp-path))
  (spit (io/file tmp-path "my-app-my-secret") "abc")
  (spit (io/file tmp-path "my-secret") "def")
  (helpers/setenv {"CREDENTIALS_DIRECTORY" tmp-path})
  (is (= "abc"
         (-> {:prefix "my-app"}
             config/create
             systemd-creds/add-provider
             (config/get :my/secret))))
  (is (= "def"
         (-> {:prefix "my-app"}
             config/create
             (systemd-creds/add-provider nil)
             (config/get :my/secret)))))
