# config

<!-- badges -->
[![cljdoc badge](https://cljdoc.org/badge/com.lambdaisland/config)](https://cljdoc.org/d/com.lambdaisland/config) [![Clojars Project](https://img.shields.io/clojars/v/com.lambdaisland/config.svg)](https://clojars.org/com.lambdaisland/config)
<!-- /badges -->

Configuration library

## Features

- Merge multiple configuration sources
  - EDN file
  - Environment variables
  - Java system properties
  - Static map
  - Map inside a var or atom
  - [lambdaisland.cli](https://github.com/lambdaisland/cli) command line flags
- ConfigProvider protocol, easy to plug in additional sources
- Unopinionated base API
- Support for a specific opinionated convention which is encouraged
- Environment-specific configuration (dev, prod, staging, etc)
- Provenance tracking (know where config values came from)
- Support for hooking up reloading

<!-- installation -->
## Installation

To use the latest release, add the following to your `deps.edn` ([Clojure CLI](https://clojure.org/guides/deps_and_cli))

```
com.lambdaisland/config {:mvn/version "0.14.53"}
```

or add the following to your `project.clj` ([Leiningen](https://leiningen.org/))

```
[com.lambdaisland/config "0.14.53"]
```
<!-- /installation -->

## Rationale

`lambdaisland/config` implements a pattern we've settled on through doing lots
of different Clojure projects, about how to handle configuration, in particular
the kind of things that differ between environments (dev, test, staging, prod),
and that you might want to set or override on multiple levels.

It is highly flexible in how you configure the sources that are checked, but has
opinionated defaults, and allows plugging in custom "providers", for instance
for checking a secret store like Hashicorp Vault or Google Secret Manager.

## Gaiwan/Lambda Island Convention

This is how we handle configuration, and the approach we encourage others to
adopt. We'll get into the nitty gritty of the library and how to do other things
with it down below.

When creating a new Clojure application, we pick a short symbolic name for it,
for this example we'll use `app-name`. `lambdaisland/config` refers to this as
the "prefix" because it's used to prefix various things.

We then create a base configuration in `resources/app-name/config.edn`. We
generally use namespaced keywords in there, and try to have a sensible default
for every configuration key that is used, maybe with a comment about what it
does.

Then we create per-environment config files: `resources/app-name/dev.edn`,
`resources/app-name/prod.edn`, etc. We generally have at least dev, prod, test,
and likely also staging. Any environment-specific config that isn't a secret
should go in there and get checked in, so that running the app, either in dev or
prod or test mode, "just works", and new developers can get onboarded quickly.

Finally each person creates a `config.local.edn` at the project root. This file
does not get checked in (we gitigore `*.local.*`). Here developers can easily
make local overrides, or configure secrets.

For dev work this is usually all you need, but when it comes time to deploying
or releasing the story varies. If you are running in a cloud environment often
the easiest (sometimes the only) option you have to do per-instance
configuration is through environment variables. Or sometimes the thing you have
the most control over is the command line invocation, in which case Java system
properties are convenient. If you're releasing to the public it might make sense
to follow XDG conventions and read a `~/.config/app-name.edn` file.
`lambdaisland/config` provides all of these mechanisms out of the box.

## Usage

### Illustrative Example

```clj
(def config
  (config/create {:prefix "app-name"
                  :env :dev}))

(config/get config :http/port) ;;=> 8080
```

This will check, in order, until it's found a value:

- The `APP_NAME__HTTP__PORT` environment variable
- The `app-name.http.port` Java system property (`System/getProperty`)
- `config.local.edn` in the JVM's CWD
- `$XDG_CONFIG_HOME/app-name.edn`
- `/etc/app-name.edn`
- `app-name/dev.edn` on the CLASSPATH (e.g. under `resources`)
- `app-name/config.edn` on the CLASSPATH

To know where a given setting came from, use `config/source`

```clj
(config/source config :http/port)
;;=> `"$HTTP__PORT environment variable"
```

### Determining `:env`

You don't have to provide an `:env` key, if not it'll be determined by the
`APP_NAME__ENV` environment variable, or the `app-name.env` system property. If
neither is set and `CI=true` (an env var set by most CI providers) then env will
be `test`. If none of these apply then the default is `dev`.

If you build a Docker image you might want to bake in `-Japp-name.env=prod`, so
it doesn't try to start in dev mode. You can still use the env var to override
this for instance in a staging environment.

### Detailed Usage

`lambdaisland/config` is based on the `ConfigProvider` protocol.

```clj
(defprotocol ConfigProvider
  (-value [this k])
  
  (-source [this k])
  (-reload [this]))
```

The result of `config/create` is a three-element map. The "environment" name, a
sequence of config providers, and an atom which acts as a cache of values
already accessed.

```clj
{:env :prod
 :providers [,,,<implement ConfigProvider protocol>,,,]
 :values (atom {:http/port {:val 8080 :source "$HTTP__PORT environment variable}})
```

`:env` can be explicitly passed in, otherwise we check the `PREFIX__ENV` (e.g.
`APP_NAME__ENV`) env var, or the `prefix.env` System property (`app-name.env`). If
neither is set and the `CI` env var is true, then we default to `:test`, if not
we fall back to `:dev`.

`create` can take a number of other options besides `:env` and `:prefix`.

- `:env-vars false` - Don't check environemnt variables
- `:prefix-env true` - Include the prefix when checking environment variables,
  e.g. `APP_NAME__HTTP__PORT` instead of `HTTP_PORT` 
- `:java-system-props false` - Don't check Java system properties
- `:local-config false` - Don't check `config.local.edn`
- `:xdg-config false` - Don't check `XDG_CONFIG_HOME` (default: `~/.config`)

If you want a different precedence order, or want to inject your own
`ConfigProvider`, then either don't use `create` and construct your own config
map as you see fit, or do use `create`, but subsequently update the `:providers`
list.

### Aero Support

All EDN files are read with [Aero](https://github.com/juxt/aero), so reader
macros like `#env`, `#profile`, and `#or` are available. We pass the app env
(`prod`, `dev`, etc) in as the Aero `:profile`.

### `lambdaisland/cli` integration

Apart from the options listed, you might also want to provide command line flags
to set specific options. For this you can use
[com.lambdaisland/cli](https://github.com/lambdaisland/cli), and
`lambdaisland.config.cli/add-provider`.

Here's an illustrative example. Note that this relies on the dynamic var
`lambdaisland.cli/*opts*` which gets bound while the command handler is being
invoked. For simple use cases this is fine. In more complex (in particular
multi-threaded) scenarios you can pass a var or other derefable to
`add-provider` explicitly.

```clj
(ns app-name
  (:require
   [lambdaisland.cli :as cli]
   [lambdaisland.config :as config]
   [lambdaisland.config.cli :as config-cli]))

(def cfg
  (-> (config/create {:prefix "app-name"})
      config-cli/add-provider))

(defn inspect-cmd
  "Inspect the `opts` coming from lambdaisland/cli"
  [opts]
  (println (str "Port=" (config/get cfg :http/port) " read from " (config/source cfg :http/port)))
  (prn opts))

(def cmdspec
  {:name "app-name"
   :doc "Illustrate cli+config usage"
   :commands
   ["inspect" #'inspect-cmd]
   :flags
   ["--port <port>" {:key :http/port
                     :doc "HTTP port to listen on"}]})

(defn -main [& argv]
  (cli/dispatch* cmdspec argv))
```

### systemd-creds integration

When running an application under Systemd, the natural way to provide API keys
or other credentials to the application is through systemd-creds. This allows
you to store the secret in an encrypted form, using a TPM module if available,
or a encryption key only accessible to root. The secrets are then decrypted and
made available to the application as files under the `$CREDENTIALS_DIRECTORY`,
only accessible to that process.

Note that this provider simply expects files following a certain naming
convention under `$CREDENTIALS_DIRECTORY`. In other words, you don't have to use
it with systemd-creds, and you can set that environment variable yourself if you
have files with secrets under a specific location, for instance in a managed
cloud environment.

To set this up, require `lambdaisland.config.systemd-creds`, and wrap the result
of `config/create` in a call to `add-provider`.

```clojure
(ns my-app.config
  (:refer-clojure :exclude [get])
  (:require
   [lambdaisland.config :as config]
   [lambdaisland.config.systemd-creds :as system-creds]))

(def prefix "my-app")

(def config
  (-> {:prefix prefix}
      config/create
      system-creds/add-provider))
      
(config/get config :api/key) ;; will look for $CREDENTIALS_DIRECTORY/my-app-api-key
```

You can pass an alternative prefix to `add-provider`, or `nil` to not use a prefix.

```clojure
(def config
  (-> {:prefix prefix}
      config/create
      (system-creds/add-provider nil)))
      
(config/get config :api/key) ;; $CREDENTIALS_DIRECTORY/api-key
```

For more info on `systemd-creds` see these man pages:

```shell
# Creating credentials
man systemd-creds

# Using LoadCredential/LoadCredentialEncrypted in service files
man systemd.exec
```

As an example, we'll create a `:api/key` credential for prefix `"my-app"`

First, create the credential, store it in the default location, this simplies
our service file. Follow naming convention `prefix-key` with `/` replaced with
`-`.

```shell
echo "my-secret" | systemd-creds encrypt - /etc/credstore.encrypted/my-app-api-key
```

In `/etc/systemd/system/my-app.service`

```conf
[Service]
User=my-app
ExecStart=clojure ...
# Make sure `resources/<prefix>/prod.edn` config defaults are used 
Environment=MY_APP__ENV=prod
LoadCredentialEncrypted=my-app-api-key
```

Using the example setup above, you can now access this credential using:

```clojure
(config/get config :api/key)
```

Note that, when using encrypted credentials, the name is stored as part of the
credential file. If you try to expose it to the application under a different
name, it will refuse to do so.

```conf
LoadCredentialEncrypted=name-in-app:name-on-disk
```

Either pass the correct name to `systemd-creds encrypt --name="name" ...`, or
provide a blank name, to disable the name check (less secure).
`systemd-creds encrypt --name= ...`.

The file name is derived from the key as follows:

- slashes become dashes
- prefix is separated from key with `-`
- characters `*?<>!` are stripped
- e.g. prefix: \"my-app\", key: `:service/api-key!` -> my-app-service-api-key

## Idiomatic Usage

In summary, the general idea is:

- Create a `resources/<prefix>/config.edn` file with your base config. Whenever
  adding a new config key it's a good idea to add a sensible default here
- Create a `resources/<prefix>/<env>.edn` for each environment. This way you can
  check in sensible `dev.edn` settings, and separate `prod.edn` settings.
- During development, create `config.local.edn` so you can easily change
  settings locally. Add `*.local.*` to `.gitignore`.
- When deploying/running in specific settings, use whatever makes most sense in
  that context to customize the deployment. If you're running through some cloud
  or lambda provider env vars might be your main option. If you control the
  `clojure` or `java` command line invocation, then system props might be handy.
  If you want a file on the filesystem you can look at and tweak, then the
  `XDG_CONFIG_HOME` convention is useful.
  
At the end of the boot process it can be a good idea to print/log
`config/sources` or `config/entries` (perhaps with
`clojure.pprint/print-table`), so when you go in to debug things you have a
record of where various configuration items are coming from.

<!-- opencollective -->
## Lambda Island Open Source

Thank you! config is made possible thanks to our generous backers. [Become a
backer on OpenCollective](https://opencollective.com/lambda-island) so that we
can continue to make config better.

<a href="https://opencollective.com/lambda-island">
<img src="https://opencollective.com/lambda-island/organizations.svg?avatarHeight=46&width=800&button=false">
<img src="https://opencollective.com/lambda-island/individuals.svg?avatarHeight=46&width=800&button=false">
</a>
<img align="left" src="https://github.com/lambdaisland/open-source/raw/master/artwork/lighthouse_readme.png">

&nbsp;

config is part of a growing collection of quality Clojure libraries created and maintained
by the fine folks at [Gaiwan](https://gaiwan.co).

Pay it forward by [becoming a backer on our OpenCollective](http://opencollective.com/lambda-island),
so that we continue to enjoy a thriving Clojure ecosystem.

You can find an overview of all our different projects at [lambdaisland/open-source](https://github.com/lambdaisland/open-source).

&nbsp;

&nbsp;
<!-- /opencollective -->

<!-- contributing -->
## Contributing

We warmly welcome patches to config. Please keep in mind the following:

- adhere to the [LambdaIsland Clojure Style Guide](https://nextjournal.com/lambdaisland/clojure-style-guide)
- write patches that solve a problem 
- start by stating the problem, then supply a minimal solution `*`
- by contributing you agree to license your contributions as MPL 2.0
- don't break the contract with downstream consumers `**`
- don't break the tests

We would very much appreciate it if you also

- update the CHANGELOG and README
- add tests for new functionality

We recommend opening an issue first, before opening a pull request. That way we
can make sure we agree what the problem is, and discuss how best to solve it.
This is especially true if you add new dependencies, or significantly increase
the API surface. In cases like these we need to decide if these changes are in
line with the project's goals.

`*` This goes for features too, a feature needs to solve a problem. State the problem it solves first, only then move on to solving it.

`**` Projects that have a version that starts with `0.` may still see breaking changes, although we also consider the level of community adoption. The more widespread a project is, the less likely we're willing to introduce breakage. See [LambdaIsland-flavored Versioning](https://github.com/lambdaisland/open-source#lambdaisland-flavored-versioning) for more info.
<!-- /contributing -->

<!-- license -->
## License

Copyright &copy; 2024 Arne Brasseur and Contributors

Licensed under the term of the Mozilla Public License 2.0, see LICENSE.
<!-- /license -->
