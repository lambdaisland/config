(ns lambdaisland.config.systemd-creds
  "Provider for systemd-creds style credentials. These are configured in .service
  files with LoadCredential, LoadCredentialEncrypted, ImportCredential,
  ImportEncryptedCredential, see `man 5 systemd.exec`"
  (:require
   clojure.java.shell
   [lambdaisland.config :as config]
   [clojure.string :as str]
   [clojure.java.io :as io]))

(defn cred-file [k]
  (when-let [cred-dir (System/getenv "CREDENTIALS_DIRECTORY")]
    (let [f (io/file cred-dir (str/replace (symbol k) #"/" "--"))]
      (when (.exists f)
        f))))

(deftype SystemdCredsProvider []
  config/ConfigProvider
  (-value [this k]
    (some-> k cred-file slurp))
  (-source [this k]
    (some-> k cred-file str))
  (-reload [this]))

(defn add-provider
  ([config]
   (reset! (:values config) {})
   (update config :providers
           into
           [(->SystemdCredsProvider)])))
