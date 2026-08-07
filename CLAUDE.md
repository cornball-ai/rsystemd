# rsystemd

Read-only systemd introspection for Runix (units, timers, journal, system state).
**No mutation APIs in Phase 1** — `systemd_start()` and friends wait for Phase 2.

API contracts, backend candidates, and testing strategy:
`~/runix/docs/phase1-introspection-contracts.md` — read it before adding functions.

Conventions:
- Exported listing functions return plain `data.frame`s with documented, stable columns.
- Fail-closed parsing: unparseable backend output is a typed error (`rsystemd_error`,
  inheriting `runix_error`), never a guess. Absent systemd is an error, not empty.
- Bridge backend is the JSON machine output (`systemctl --output=json`,
  `journalctl -o json`), never human text; sd-bus/sd-journal are the native successors.
- Injectable runners: exported functions never call `system2()` directly; tests
  substitute fakes. Fixtures in `inst/tinytest/fixtures/`, live smoke tests behind
  `tinytest::at_home()`.
- Tinyverse workflow: `tinyrox::document()`, `tinypkgr::install()`, `tinytest`.
