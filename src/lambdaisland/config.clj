(ns lambdaisland.config
  (:require
   [aero.core :as aero]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [lambdaisland.data-printers :as printers]))

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
  (str (str/upper-case (str/replace prefix #"/" "_"))
       (if (string? k)
         k
         (str (when (qualified-ident? k)
                (str (str/upper-case (munge (namespace k)))
                     "__"))
              (str/upper-case (munge (name k)))))))

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

(deftype AeroProvider [path opts cache]
  ConfigProvider
  (-value [this k]
    (when-not @cache
      (reset! cache (aero/read-config path opts)))
    (get @cache k))
  (-source [this k]
    (cond
      (instance? java.io.File path)
      (.getCanonicalPath ^java.io.File path)
      (string? path)
      (.getCanonicalPath ^java.io.File (io/file path))
      :else
      path))
  (-reload [this]
    (reset! cache (aero/read-config path opts)))
  clojure.lang.IMeta
  (meta [this]
    {:path path
     :opts opts}))

(deftype EnvProvider [prefix]
  ConfigProvider
  (-value [this k] (System/getenv (key->env-var prefix k)))
  (-source [this k] (str "(System/getenv " (pr-str (key->env-var prefix k)) ")"))
  (-reload [this])
  clojure.lang.IMeta
  (meta [this]
    {:prefix prefix}))

(defn prop-key [prefix k]
  (str/replace (str prefix "." (subs (str k) 1)) #"/" "."))

(deftype PropertiesProvider [prefix]
  ConfigProvider
  (-value [this k] (System/getProperty (prop-key prefix k)))
  (-source [this k]  (str "(System/getProperty " (pr-str (prop-key prefix k)) ")"))
  (-reload [this])
  clojure.lang.IMeta
  (meta [this]
    {:prefix prefix}))

(doseq [c [AeroProvider EnvProvider PropertiesProvider]]
  (printers/register-print c (.getName c) meta)
  (printers/register-pprint c (.getName c) meta))

(defn new-config [providers]
  {:providers (remove nil? providers)
   :values (atom {})})

(defn create [{:keys [prefix env-vars java-system-props local-config xdg-config] :as opts
               :or   {env-vars          true
                      java-system-props true
                      local-config      true
                      xdg-config        true}}]
  (let [env          (env-key opts)
        config-edn   (io/resource (str prefix "/config.edn"))
        env-edn      (io/resource (str prefix "/" (name env) ".edn"))
        config-local (io/file "config.local.edn")
        aero-opts    {:profile env}
        xdg-path     (io/file
                      (or (System/getenv "XDG_CONFIG_HOME")
                          (io/file (System/getProperty "user.home") ".config"))
                      (str prefix ".edn"))]
    (new-config
     [(when config-edn
        (->AeroProvider config-edn aero-opts (atom nil)))
      (when env-edn
        (->AeroProvider env-edn aero-opts (atom nil) ))
      (when (and xdg-config (.exists xdg-path))
        (->AeroProvider xdg-path aero-opts (atom nil)))
      (when java-system-props
        (->PropertiesProvider prefix))
      (when (and local-config (.exists config-local))
        (->AeroProvider config-local aero-opts (atom nil)))
      (when env-vars
        (->EnvProvider prefix))])))

(defn value
  ([config k]
   (value config k nil))
  ([{:keys [providers values] :as config} k fallback]
   (let [vs @(:values config)]
     (if (contains? vs k)
       (get-in vs [k :val])
       (let [[p v] (first (filter (comp some? second) (map (fn [p] [p (-value p k)]) providers)))]
         (swap! values assoc k {:val v :source p})
         (if (some? v)
           v
           fallback))))))

(defn source [{:keys [providers values] :as config} k]
  (let [vs @(:values config)]
    (if (contains? vs k)
      (-source (get-in vs [k :source]) k)
      (let [[p v] (first (filter (comp some? second) (map (fn [p] [p (-value p k)]) providers)))]
        (swap! values assoc k {:val v :source p})
        (-source p v)))))

(defn sources [config]
  (into {}
        (map (fn [[k {v :val p :source}]]
               [k (-source p k)]))
        @(:values config)))

(defn values [config]
  (into {}
        (map (fn [[k {v :val}]]
               [k v]))
        @(:values config)))

(comment
  (def c (create {:prefix "foo/bar"}))

  (value c :test)
  (value c :baz)

  (sources c)
  (values c)

  (System/getProperty
   (prop-key "foo/bar" :baz))

  (-value
   (->PropertiesProvider "foo/bar")
   :baz)

  (System/setProperty "foo.bar.baz" "bsq"))
