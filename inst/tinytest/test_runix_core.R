## Compatibility after adopting the runix core: the condition taxonomy, the
## injectable runner/sleeper hooks the suite depends on, and the result shell
## all behave as before.

## --- injectable runner and sleeper round-trip (the fixture hooks) ---
fake_run <- function(cmd, args) list(status = 0L, output = "x", stderr = character())
old <- rsystemd:::set_runner(fake_run)
expect_identical(rsystemd:::runner(), fake_run)
rsystemd:::set_runner(old)
expect_false(identical(rsystemd:::runner(), fake_run))

fake_sleep <- function(s) invisible(NULL)
olds <- rsystemd:::set_sleeper(fake_sleep)
expect_identical(rsystemd:::sleeper(), fake_sleep)
rsystemd:::set_sleeper(olds)
expect_false(identical(rsystemd:::sleeper(), fake_sleep))

## --- typed conditions keep the class taxonomy ---
e <- tryCatch(rsystemd:::stop_rsystemd("boom", class = "rsystemd_bad_unit"),
              condition = function(c) c)
expect_inherits(e, "rsystemd_bad_unit")
expect_inherits(e, "rsystemd_error")
expect_inherits(e, "runix_error")
expect_equal(conditionMessage(e), "boom")

## --- stop_mutation merges structured data onto the condition ---
m <- tryCatch(
    rsystemd:::stop_mutation("timed out", class = "rsystemd_timeout",
                             data = list(resource = "cups.service",
                                         observed = "activating", elapsed = 90)),
    condition = function(c) c)
expect_inherits(m, "rsystemd_timeout")
expect_inherits(m, "rsystemd_error")
expect_inherits(m, "runix_error")
expect_equal(m$resource, "cups.service")
expect_equal(m$observed, "activating")
expect_equal(m$elapsed, 90)

## --- the default runner fails closed on a missing tool, rsystemd-typed ---
mt <- tryCatch(
    rsystemd:::run_system("rsystemd-no-such-tool-xyz", character()),
    condition = function(c) c)
expect_inherits(mt, "rsystemd_missing_tool")
expect_inherits(mt, "rsystemd_error")
expect_inherits(mt, "runix_error")
expect_equal(conditionMessage(mt),
             "backend tool not found: rsystemd-no-such-tool-xyz")

## --- the result shell keeps class, fields, audit, and print ---
r <- rsystemd:::new_systemd_result(
    "systemd.restart", "cups.service", changed = TRUE, state_changed = TRUE,
    preview = FALSE, before = list(active_state = "active"),
    after = list(active_state = "active"),
    planned = list(effect_would_issue = TRUE),
    completion = list(method = "invocation_id", job_result = "done"),
    authorized_via = "polkit:org.freedesktop.systemd1.manage-units",
    outcome = "ok")
expect_inherits(r, "systemd_result")
expect_inherits(r, "runix_result")
expect_equal(r$operation, "systemd.restart")
expect_equal(r$completion$method, "invocation_id")
expect_equal(r$audit$outcome, "ok")
expect_equal(r$audit$authorized_via,
             "polkit:org.freedesktop.systemd1.manage-units")
out <- capture.output(print(r))
expect_true(any(grepl("restart cups.service", out)))
