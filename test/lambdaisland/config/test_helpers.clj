(ns lambdaisland.config.test-helpers
  (:import (jnr.posix POSIXFactory)))

(set! *warn-on-reflection* true)

(defn- accessible-field ^java.lang.reflect.Field [^Class klz field]
  (doto (.getDeclaredField klz field)
    (.setAccessible true)))

(defn- get-static [field]
  (let [klz (Class/forName (namespace field))]
    (.get (accessible-field klz (name field)) klz)))

(defn- get-field [^Object instance field]
  (.get (accessible-field (.getClass instance) (str field)) instance))

(defn- set-field! [klz field obj val]
  (.set (accessible-field klz field) obj val))

(def ^java.util.Map theEnvironment
  (get-static 'java.lang.ProcessEnvironment/theEnvironment))

(def ^java.lang.ProcessEnvironment$StringEnvironment theUnmodifiableEnvironment
  (get-field (get-static 'java.lang.ProcessEnvironment/theUnmodifiableEnvironment) 'm))

(def ^jnr.posix.POSIX posix (POSIXFactory/getPOSIX))

(defn setenv
  ([env]
   (run! (fn [[k v]] (setenv k v)) env))
  ([^String var ^String val]
   (.put theEnvironment var val)
   (.put theUnmodifiableEnvironment var val)
   (.setenv posix var val 1)))
