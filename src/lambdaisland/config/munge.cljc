(ns lambdaisland.config.munge
  "Logic for how to turn a config key and possibly an app prefix into various
  things we can look up.

  Env vars:
  - uppercased
  - dashes and slashes become underscores
  - prefix is separated from key with `__`
  - special characters are munged see ([[clojure.core/munge]])
  - e.g. prefix: \"my-app\", key: `:service/api-key` -> MY_APP__SERVICE_API_KEY

  File names:
  - slashes become dashes
  - prefix is separated from key with `-`
  - characters `*?<>!` are stripped
  - e.g. prefix: \"my-app\", key: `:service/api-key!` -> my-app-service-api-key
  "
  (:require [clojure.string :as str]))

(defn env-case [s]
  (-> s str/upper-case (str/replace #"[-/]" "_")))

(defn key->env-var
  "Take the key used to identify a setting or secret, and turn it into a string
  suitable for use as an environment variable.

  - if the key is already a string it is left untouched
  - otherwise it is assumed to be an ident (symbol or keyword)
  - identifiers are uppercased and munged, as per [[munge]]
  - dashes become underscores
  - if the ident is qualified (has a namespace), two underscores are used to
    separate name and namespace"
  [prefix k]
  (str
   (when prefix
     (str (env-case prefix) "__"))
   (if (string? k)
     k
     (str (when (qualified-ident? k)
            (str (str/upper-case (munge (namespace k)))
                 "__"))
          (str/upper-case (munge (name k)))))))

(defn file-name [prefix k]
  (str/replace
   (str
    prefix
    (when prefix "-")
    (str/replace (symbol k) #"/" "-"))
   #"[\*\?<>!]" ""))
