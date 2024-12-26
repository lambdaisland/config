# Unreleased

## Added

## Fixed

## Changed

# 0.4.17 (2024-12-26 / 139a028)

## Added

- Also check `/etc/<app-name>.edn`

## Changed

- Change the priority of Java system properties, they come right after env vars,
  overriding config.local.edn, XDG config, etc.

# 0.3.13 (2024-12-23 / db97476)

## Changed

- Make sure command line flags override other config sources

# 0.2.10 (2024-12-23 / 51ef5bd)

## Added

- Add MapProvider, DerefMapProvider, and integration with lambdaisland.cli

# 0.1.6 (2024-12-17 / 0e8e619)

## Added

- First release
