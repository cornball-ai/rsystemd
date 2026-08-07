# rsystemd

Read-only introspection of systemd from R: units, timers, unit properties,
system state, and journal entries, returned as plain data frames.

Part of [Runix](https://github.com/cornball-ai/runix), an R-native Unix
systems-administration framework.

**Status: experimental.** Version 0.0.1.x; the API changes without
deprecation until 0.1.0. Mutation APIs (`systemd_start()` and friends) are
deliberately absent — they arrive in a later Runix phase.

Implemented so far:

- `systemd_units(pattern = NULL)` — all loaded units (including inactive),
  optionally filtered by a glob pattern.

Backend note: currently bridges systemctl's JSON machine output
(`--output=json`, systemd ≥ 249); native sd-bus/sd-journal backends are the
plan, behind the same R API. Design rules, per the
[Phase 1 contracts](https://github.com/cornball-ai/runix/blob/master/docs/phase1-introspection-contracts.md):
plain data frames with stable columns, fail-closed parsing, injectable
runners so tests run offline against recorded fixtures.

## Install

```r
remotes::install_github("cornball-ai/rsystemd")
```

Linux with systemd only (`OS_type: unix`).

## License

MIT © cornball.ai
