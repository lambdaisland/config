(ns lambdaisland.config
  (:require
   [aero.core :as aero]
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn key->env-var
  "Take the key used to identify a setting or secret, and turn it into a string
  suitable for use as an environment variable.

  - if the key is already a string it is left untouched
  - otherwise it is assumed to be an ident (symbol or keyword)
  - identifiers are uppercased and munged, as per [[munge]]
  - dashes become underscores
  - if the ident is qualified (has a namespace), two underscores are used to
    separate name and namespace"
  [k]
  (if (string? k)
    k
    (str (when (qualified-ident? k)
           (str (str/upper-case (munge (namespace k)))
                "__"))
         (str/upper-case (munge (name k))))))

(defn env-key
  "The current environment name, as a keyword, for instance `:dev`, `:prod`, or `:test`

  Checked in order
  - `:env` explicitly passed in
  - `PREFIX_ENV` env var, based on the configured `:prefix`, uppercased
  - `prefix.env` Java system property (use Java CLI flag `-D`, e.g. `-Dprefix.env=prod`)
  - if env var `CI=true` then `:test` (convention used by most CI providers)
  - otherwise: `:dev`"
  [opts]
  (or
   (:env opts)
   (some-> (:prefix opts) (str/replace #"/" "_") str/upper-case (str "_ENV") System/getenv keyword)
   (some-> (:prefix opts) (str/replace #"/" ".") (str ".env") System/getProperty keyword)
   (when (= "true" (System/getenv "CI")) :test)
   :dev))

(defprotocol ConfigProvider
  (-value [this k])
  (-source [this k])
  (-reload [this]))

(defn ensure-aero [path cache opts]
  (when-not @cache
    (reset! cache (aero/read-config path opts))))

(deftype AeroProvider [path cache opts]
  ConfigProvider
  (-value [this k]
    (when-not @cache
      (reset! cache (aero/read-config path opts)))
    (get @cache k))
  (-source [this k]
    (str k " in " path))
  (-reload [this]
    (reset! cache (aero/read-config path opts))))

(deftype EnvProvider []
  ConfigProvider
  (-value [this k] (System/getenv (key->env-var k)))
  (-source [this k] (str (key->env-var k) " environment variable"))
  (-reload [this]))

(deftype PropertiesProvider []
  ConfigProvider
  (-value [this k] (System/getProperty (str/replace (subs (str k) 1) #"/" ".")))
  (-source [this k] (str (str/replace (subs (str k) 1) #"/" ".") " java system property"))
  (-reload [this]))

(defn create [{:keys [prefix env-vars java-system-props local-config xdg-config] :as opts
               :or   {env-vars true
                      java-system-props true
                      local-config true
                      xdg-config true}}]
  (let [env          (env-key opts)
        config-edn   (io/resource (str prefix "/config.edn"))
        env-edn      (io/resource (str prefix "/" (name env) ".edn"))
        config-local (io/file "config.local.edn")
        aero-opts    {:profile env}
        xdg-path  (io/file
                   (or (System/getenv "XDG_CONFIG_HOME")
                       (io/file (System/getProperty "user.home") ".config"))
                   (str prefix ".edn"))]
    {:providers
     (remove nil?
             [(when config-edn
                (->AeroProvider config-edn aero-opts (atom nil)))
              (when env-edn
                (->AeroProvider env-edn aero-opts (atom nil) ))
              (when java-system-props
                (->PropertiesProvider))
              (when (and xdg-config (.exists xdg-path))
                (->AeroProvider xdg-path aero-opts (atom nil)))
              (when (and local-config (.exists config-local))
                (->AeroProvider config-local aero-opts (atom nil)))
              (when env-vars
                (->EnvProvider))
              ])
     :values (atom {})}
    ))

(create {:prefix "foo/bar"})
