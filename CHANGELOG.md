# Unreleased

## Fixed

- Convert dashes in prefix to underscore when used as env-var for getting PREFIX__ENV

# 0.11.42 (2026-07-01 / f826ee0)

## Added

- Add support for systemd-creds ($CREDENTIALS_DIRECTORY)

# 0.10.37 (2025-08-25 / 262e81e)

## Fixed

- Handle `false` values correctly

# 0.9.34 (2025-06-23 / a8ef842)

## Fixed

- fix reader conditional error

# 0.6.27 (2025-04-23 / e7e97b4)

## Fixed

- bb compatibility

# 0.5.22 (2025-02-06 / 9db9c06)

## Changed

- Bump data-printers, and use the new `auto` functionality

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
