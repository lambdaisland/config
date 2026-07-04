(ns lambdaisland.config.systemd-creds
  "Provider for systemd-creds style credentials. These are configured in .service
  files with LoadCredential, LoadCredentialEncrypted, ImportCredential,
  ImportEncryptedCredential, see `man 5 systemd.exec`"
  (:require
   clojure.java.shell
   [lambdaisland.config.munge :as munge]
   [lambdaisland.config :as config]
   [clojure.string :as str]
   [clojure.java.io :as io]))

(defn cred-file [prefix k]
  (when-let [cred-dir (System/getenv "CREDENTIALS_DIRECTORY")]
    (let [f (io/file cred-dir (munge/file-name prefix k))]
      (when (.exists f)
        f))))

(deftype SystemdCredsProvider [prefix]
  config/ConfigProvider
  (-value [this k]
    (some->> k (cred-file prefix) slurp))
  (-source [this k]
    (some->> k (cred-file prefix) str))
  (-reload [this]))

(defn add-provider
  ([config]
   (add-provider config (:prefix config)))
  ([config prefix]
   (reset! (:values config) {})
   (update config :providers
           into
           [(->SystemdCredsProvider prefix)])))
