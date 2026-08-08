# systemd mutation verbs: the six required conformance scenarios (Phase 2
# contract) plus preview, idempotence, and input validation. All fixture-
# driven through a stateful fake runner; no live mutation.

options(rsystemd.poll_interval = 0) # no real sleeping between polls

# A scriptable fake systemctl. `show_states` is a list of observed-field
# lists returned by successive `show` calls (the last one repeats); effect
# calls (start/stop/restart/enable/disable) return `effect_status` and
# advance the show cursor past `advance_after` so state "settles" only
# after the job runs. Renders the observed list back into show output.
fake_systemctl <- function(show_states, effect_status = 0L,
    effect_stderr = character(), advance_after = 1L) {
    st <- new.env()
    st$i <- 1L
    st$issued <- FALSE
    render <- function(s) {
        c(
            paste0("Id=", s$unit %||% "x.service"),
            "Description=fake",
            paste0("LoadState=", s$load_state %||% "loaded"),
            paste0("ActiveState=", s$active_state),
            paste0("SubState=", s$sub_state %||% "running"),
            paste0("UnitFileState=", s$unit_file_state %||% "enabled"),
            "FragmentPath=/x",
            "ActiveEnterTimestamp=",
            paste0("MainPID=", s$main_pid %||% 0L),
            "MemoryCurrent=[not set]",
            "NRestarts=0",
            paste0("InvocationID=", s$invocation_id %||% ""),
            paste0("StateChangeTimestampMonotonic=", s$scm %||% 1000L)
        )
    }
    function(cmd, args) {
        is_show <- any(args == "show") # scope-aware: --user may precede
        if (is_show) {
            idx <- min(st$i, length(show_states))
            out <- render(show_states[[idx]])
            if (st$issued) st$i <- st$i + 1L
            list(status = 0L, output = out, stderr = character())
        } else {
            # an effect call
            st$issued <- TRUE
            st$i <- st$i + advance_after - 1L
            list(status = effect_status, output = character(),
                stderr = effect_stderr)
        }
    }
}
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

svc <- function(active, inv, scm = 1000L, ufs = "enabled",
    sub = "running", pid = 0L) {
    list(unit = "cups.service", active_state = active, sub_state = sub,
        unit_file_state = ufs, main_pid = pid, invocation_id = inv,
        scm = scm)
}

# --- 1. restart of an already-active service with STALE active ----------
# before active(inv=A); after the effect a fresh invocation B appears. The
# verb must not accept the stale A read as success.

old <- rsystemd:::set_runner(fake_systemctl(
    list(svc("active", "AAAA"),       # before + first (stale) poll
         svc("active", "BBBB", scm = 2000L)), # after job runs
    advance_after = 2L))
r <- systemd_restart("cups.service", timeout = 5)
rsystemd:::set_runner(old)
expect_equal(r$operation, "systemd.restart")
expect_true(r$changed)
expect_true(r$state_changed)
expect_equal(r$completion$method, "invocation_id")
expect_equal(r$completion$invocation_before, "AAAA")
expect_equal(r$completion$invocation_after, "BBBB")
expect_equal(r$audit$outcome, "ok")

# --- 2. oneshot restart with MainPID = 0 --------------------------------
# oneshot returns active/exited, no PID; correlation via InvocationID only.

old <- rsystemd:::set_runner(fake_systemctl(
    list(svc("active", "C1", sub = "exited", pid = 0L),
         svc("active", "C2", sub = "exited", pid = 0L, scm = 2000L)),
    advance_after = 2L))
r <- systemd_restart("oneshot.service", timeout = 5)
rsystemd:::set_runner(old)
expect_true(r$changed)
expect_true(is.na(r$after$main_pid))
expect_equal(r$completion$invocation_after, "C2")

# --- 3. invocation marker never advances -> submitted -------------------

old <- rsystemd:::set_runner(fake_systemctl(
    list(svc("active", "SAME")), advance_after = 1L))
r <- systemd_restart("stuck.service", timeout = 0.05)
rsystemd:::set_runner(old)
expect_true(is.na(r$changed))
expect_true(is.na(r$state_changed))
expect_equal(r$completion$method, "submitted_unconfirmed")
expect_equal(r$audit$outcome, "submitted")

# --- 4. job failure after submission ------------------------------------

old <- rsystemd:::set_runner(fake_systemctl(
    list(svc("inactive", NA_character_),
         svc("failed", "F1", scm = 2000L)),
    advance_after = 2L))
e <- tryCatch(systemd_start("bad.service", timeout = 5), error = identity)
rsystemd:::set_runner(old)
expect_inherits(e, "runix_operation_failed")
expect_inherits(e, "rsystemd_error")
expect_equal(e$observed$active_state, "failed")

# --- 5. cancellation during polling -------------------------------------
# the injected sleeper raises an interrupt-classed condition on first wait.

old <- rsystemd:::set_runner(fake_systemctl(
    list(svc("active", "AAAA")), advance_after = 1L))
oldslp <- rsystemd:::set_sleeper(function(t) {
    stop(structure(
        class = c("rsystemd_interrupt", "rsystemd_error", "error",
            "condition"),
        list(message = "cancel", call = NULL)))
})
e <- tryCatch(systemd_restart("cups.service", timeout = 5),
    error = identity)
rsystemd:::set_sleeper(oldslp)
rsystemd:::set_runner(old)
expect_inherits(e, "runix_cancelled")
expect_false(is.null(e$observed))
expect_equal(e$observed$active_state, "active")

# --- 6. timeout with truthful observed ----------------------------------
# start of an inactive unit that never becomes active: times out, but the
# observed post-state is real (still activating).

old <- rsystemd:::set_runner(fake_systemctl(
    list(svc("inactive", NA_character_),
         svc("activating", "T1", sub = "start")),
    advance_after = 2L))
e <- tryCatch(systemd_start("slow.service", timeout = 0.05),
    error = identity)
rsystemd:::set_runner(old)
expect_inherits(e, "runix_timeout")
expect_true(e$elapsed >= 0)
expect_equal(e$observed$active_state, "activating")
expect_false(e$observed_failed)

# --- preview issues no effect ------------------------------------------

seen <- new.env()
seen$effect <- FALSE
probe <- function(cmd, args) {
    if (!any(args == "show")) seen$effect <- TRUE
    fake_systemctl(list(svc("inactive", NA_character_)))(cmd, args)
}
old <- rsystemd:::set_runner(probe)
r <- systemd_start("cups.service", dry_run = TRUE)
rsystemd:::set_runner(old)
expect_true(r$preview)
expect_true(r$changed) # would change (inactive -> active)
expect_true(is.na(r$after))
expect_false(seen$effect)
expect_equal(r$completion$method, "preview")

# --- idempotence: start an already-active unit issues no effect ---------

seen$effect <- FALSE
old <- rsystemd:::set_runner(probe2 <- function(cmd, args) {
    if (!any(args == "show")) seen$effect <- TRUE
    fake_systemctl(list(svc("active", "AAAA")))(cmd, args)
})
r <- systemd_start("cups.service")
rsystemd:::set_runner(old)
expect_false(r$changed)
expect_false(r$state_changed)
expect_false(seen$effect)
expect_equal(r$completion$method, "noop")

# --- stopping a failed unit: state_changed TRUE, changed FALSE ----------

old <- rsystemd:::set_runner(fake_systemctl(
    list(svc("failed", "X1"),
         svc("inactive", NA_character_, scm = 2000L)),
    advance_after = 2L))
r <- systemd_stop("crashed.service", timeout = 5)
rsystemd:::set_runner(old)
expect_false(r$changed)          # was not running functionally
expect_true(r$state_changed)     # failed -> inactive is a real transition

# --- authorization denial -> runix_unauthorized -------------------------

old <- rsystemd:::set_runner(fake_systemctl(
    list(svc("inactive", NA_character_)),
    effect_status = 1L,
    effect_stderr = "Interactive authentication required.",
    advance_after = 1L))
e <- tryCatch(systemd_start("cups.service", timeout = 5), error = identity)
rsystemd:::set_runner(old)
expect_inherits(e, "runix_unauthorized")
expect_equal(e$polkit_action, "org.freedesktop.systemd1.manage-units")

# --- enable is synchronous, idempotent when already enabled -------------

old <- rsystemd:::set_runner(fake_systemctl(
    list(svc("active", "AAAA", ufs = "disabled"),
         svc("active", "AAAA", ufs = "enabled")),
    advance_after = 2L))
r <- systemd_enable("cups.service")
rsystemd:::set_runner(old)
expect_true(r$changed)
expect_equal(r$completion$method, "synchronous")

old <- rsystemd:::set_runner(fake_systemctl(list(svc("active", "A",
    ufs = "enabled"))))
r <- systemd_enable("cups.service")
rsystemd:::set_runner(old)
expect_false(r$changed)
expect_equal(r$completion$method, "noop")

# --- scope regression: --user must reach BOTH observation and effect ----
# A fake that ignored args once hid a real bug (effect was --user, observe
# was system-scope, so correlation silently failed live). This records
# every argv and asserts scope consistency across show AND the effect.

record_argv <- function(states) {
    calls <- new.env()
    calls$argv <- list()
    st <- new.env()
    st$i <- 1L
    st$issued <- FALSE
    fake <- fake_systemctl(states, advance_after = 2L)
    list(
        env = calls,
        fn = function(cmd, args) {
            calls$argv[[length(calls$argv) + 1L]] <- args
            fake(cmd, args)
        }
    )
}

# user scope: every systemctl invocation must carry --user
rec <- record_argv(list(svc("active", "U1"),
    svc("active", "U2", scm = 2000L)))
old <- rsystemd:::set_runner(rec$fn)
systemd_restart("u.service", scope = "user", timeout = 5)
rsystemd:::set_runner(old)
is_show <- vapply(rec$env$argv, function(a) any(a == "show"), logical(1))
has_user <- vapply(rec$env$argv, function(a) any(a == "--user"), logical(1))
expect_true(any(is_show))                       # observation happened
expect_true(any(!is_show))                      # an effect happened
expect_true(all(has_user))                      # --user on ALL calls
expect_true(all(has_user[is_show]))             # ...including observation
expect_true(all(has_user[!is_show]))            # ...and the effect

# system scope: no call carries --user
rec <- record_argv(list(svc("active", "S1"),
    svc("active", "S2", scm = 2000L)))
old <- rsystemd:::set_runner(rec$fn)
systemd_restart("s.service", scope = "system", timeout = 5)
rsystemd:::set_runner(old)
has_user <- vapply(rec$env$argv, function(a) any(a == "--user"), logical(1))
expect_false(any(has_user))

# --- audit authorized_via reflects what actually authorized -------------
# system-scope effect -> the polkit action; preview/noop -> not_required;
# user-scope effect -> not_required (caller's own manager). Never hardcoded.

old <- rsystemd:::set_runner(fake_systemctl(
    list(svc("active", "AAAA"), svc("active", "BBBB", scm = 2000L)),
    advance_after = 2L))
r <- systemd_restart("cups.service", timeout = 5)
rsystemd:::set_runner(old)
expect_equal(r$audit$authorized_via,
    "polkit:org.freedesktop.systemd1.manage-units")

old <- rsystemd:::set_runner(fake_systemctl(
    list(svc("active", "AAAA", ufs = "disabled"),
         svc("active", "AAAA", ufs = "enabled")),
    advance_after = 2L))
r <- systemd_enable("cups.service")
rsystemd:::set_runner(old)
expect_equal(r$audit$authorized_via,
    "polkit:org.freedesktop.systemd1.manage-unit-files")

# preview: no effect issued -> not_required
old <- rsystemd:::set_runner(fake_systemctl(list(svc("inactive",
    NA_character_))))
r <- systemd_start("cups.service", dry_run = TRUE)
rsystemd:::set_runner(old)
expect_equal(r$audit$authorized_via, "not_required")

# noop: already active -> not_required
old <- rsystemd:::set_runner(fake_systemctl(list(svc("active", "AAAA"))))
r <- systemd_start("cups.service")
rsystemd:::set_runner(old)
expect_equal(r$audit$authorized_via, "not_required")

# user scope: caller's own manager, not a system polkit action
old <- rsystemd:::set_runner(fake_systemctl(
    list(svc("active", "AAAA"), svc("active", "BBBB", scm = 2000L)),
    advance_after = 2L))
r <- systemd_restart("cups.service", scope = "user", timeout = 5)
rsystemd:::set_runner(old)
expect_equal(r$audit$authorized_via, "not_required")

# --- input validation ---------------------------------------------------

expect_error(systemd_start(c("a", "b")))
expect_error(systemd_start("x", scope = "container"))
expect_error(systemd_start("x", timeout = 0))
expect_error(systemd_start("x", dry_run = NA))
