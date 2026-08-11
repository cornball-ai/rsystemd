# systemd mutation verbs: the six required conformance scenarios (Phase 2
# contract) plus preview, idempotence, and input validation. All fixture-
# driven through a stateful fake runner; no live mutation.

options(rsystemd.poll_interval = 0) # no real sleeping between polls

# Route all mutation audit to an in-memory sink so the suite stays offline
# (nothing touches the real XDG/system sink). Reset at the end of the file.
.audit_mem <- runix::memory_audit_sink()
rsystemd:::set_audit_resolver(function(scope) {
    list(sink = .audit_mem,
        audit_scope = if (identical(scope, "user")) "user" else "caller",
        system_durable_audit = FALSE)
})

# A fresh in-memory sink for a single assertion block; restores the shared
# resolver afterward. `fail_on(record)` simulates a persistence failure.
with_fresh_sink <- function(fail_on = NULL, audit_scope = "caller") {
    s <- runix::memory_audit_sink(fail_on = fail_on)
    old <- rsystemd:::set_audit_resolver(function(scope) {
        list(sink = s, audit_scope = audit_scope, system_durable_audit = FALSE)
    })
    list(sink = s, restore = function() rsystemd:::set_audit_resolver(old))
}

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
# Validation happens before any audit or effect, so these never write records.

expect_error(systemd_start(c("a", "b")))
expect_error(systemd_start("x", scope = "container"))
expect_error(systemd_start("x", timeout = 0))
expect_error(systemd_start("x", dry_run = NA))

# --- durable audit: effect path writes intent then outcome, one cid ------

fs <- with_fresh_sink()
old <- rsystemd:::set_runner(fake_systemctl(
    list(svc("active", "AAAA"), svc("active", "BBBB", scm = 2000L)),
    advance_after = 2L))
r <- systemd_restart("cups.service", timeout = 5)
rsystemd:::set_runner(old)
fs$restore()
recs <- fs$sink$records()
expect_equal(length(recs), 2L)
expect_equal(recs[[1]]$phase, "intent")
expect_equal(recs[[2]]$phase, "outcome")
expect_equal(recs[[1]]$correlation_id, recs[[2]]$correlation_id)
expect_equal(r$correlation_id, recs[[1]]$correlation_id)   # one id, result too
expect_equal(recs[[1]]$effect_issued, FALSE)               # intent: not yet
expect_equal(recs[[2]]$effect_issued, TRUE)                # outcome: issued
expect_equal(recs[[2]]$outcome, "ok")
expect_equal(r$audit_scope, "caller")
expect_true(r$audit_persisted)
# actor is sink-derived framing (runix >= 0.0.1.8): the record builder no
# longer supplies it, the sink stamps a normalized uid:N, and the in-memory
# result still carries audit$actor from the shared runix helper. This is the
# fix for the A1-canary finding -- a client-supplied actor made the broker
# reject every unprivileged system-scope mutation as schema_invalid.
expect_true(grepl("^uid:", recs[[1]]$actor))          # sink-stamped on intent
expect_true(grepl("^uid:", recs[[2]]$actor))          # and on outcome
expect_true(grepl("^uid:", r$audit$actor))            # kept on the result
# the domain-record builders never emit actor themselves (regression guard so
# a producer never again trips the broker's reserved-key rejection):
ir <- rsystemd:::intent_record("systemd.restart", "cups.service", "system",
                               "caller")
expect_false("actor" %in% names(ir))
cr <- rsystemd:::audit_record_from_condition(
    structure(list(observed = NULL, elapsed = 0), class = "runix_timeout"),
    "systemd.restart", "cups.service", "system", "caller")
expect_false("actor" %in% names(cr))

# --- preview and no-op each write exactly one non-effect record ----------

fs <- with_fresh_sink()
old <- rsystemd:::set_runner(fake_systemctl(list(svc("inactive",
    NA_character_))))
r <- systemd_start("cups.service", dry_run = TRUE)
rsystemd:::set_runner(old)
fs$restore()
recs <- fs$sink$records()
expect_equal(length(recs), 1L)
expect_equal(recs[[1]]$phase, "preview")
expect_equal(recs[[1]]$effect_issued, FALSE)
expect_equal(r$correlation_id, recs[[1]]$correlation_id)

fs <- with_fresh_sink()
old <- rsystemd:::set_runner(fake_systemctl(list(svc("active", "AAAA"))))
r <- systemd_start("cups.service")
rsystemd:::set_runner(old)
fs$restore()
recs <- fs$sink$records()
expect_equal(length(recs), 1L)
expect_equal(recs[[1]]$phase, "noop")
expect_equal(recs[[1]]$effect_issued, FALSE)

# --- fail-closed: intent not durable -> no effect issued, raises ---------

fs <- with_fresh_sink(fail_on = function(rec) identical(rec$phase, "intent"))
seen <- new.env()
seen$effect <- FALSE
probe_fc <- function(cmd, args) {
    if (!any(args == "show")) seen$effect <- TRUE
    fake_systemctl(list(svc("inactive", NA_character_),
        svc("active", "Z1", scm = 2000L)), advance_after = 2L)(cmd, args)
}
old <- rsystemd:::set_runner(probe_fc)
e <- tryCatch(systemd_start("cups.service", timeout = 5), error = identity)
rsystemd:::set_runner(old)
fs$restore()
expect_inherits(e, "runix_audit_error")
expect_false(seen$effect)                                  # gate 1: nothing issued

# --- error path: typed failure recorded richly; cid on the condition -----

fs <- with_fresh_sink()
old <- rsystemd:::set_runner(fake_systemctl(
    list(svc("inactive", NA_character_), svc("failed", "F1", scm = 2000L)),
    advance_after = 2L))
e <- tryCatch(systemd_start("bad.service", timeout = 5), error = identity)
rsystemd:::set_runner(old)
fs$restore()
expect_inherits(e, "runix_operation_failed")               # typed class survives
expect_equal(e$observed$active_state, "failed")            # observed survives
expect_false(is.null(e$correlation_id))                    # cid on the error
recs <- fs$sink$records()
expect_equal(length(recs), 2L)
expect_equal(recs[[2]]$phase, "outcome")
expect_equal(recs[[2]]$outcome, "failed")                  # rich, not generic
expect_equal(recs[[2]]$effect_issued, TRUE)
expect_equal(e$correlation_id, recs[[1]]$correlation_id)

# --- outcome-write failure must not mask the mutation result -------------

fs <- with_fresh_sink(fail_on = function(rec) identical(rec$phase, "outcome"))
old <- rsystemd:::set_runner(fake_systemctl(
    list(svc("active", "AAAA"), svc("active", "BBBB", scm = 2000L)),
    advance_after = 2L))
r <- systemd_restart("cups.service", timeout = 5)
rsystemd:::set_runner(old)
fs$restore()
expect_equal(r$operation, "systemd.restart")               # result intact
expect_true(r$changed)
expect_false(r$audit_persisted)                            # honest: not durable

# --- audit_scope reflects the resolved authority (user scope) ------------

fs <- with_fresh_sink(audit_scope = "user")
old <- rsystemd:::set_runner(fake_systemctl(
    list(svc("active", "AAAA"), svc("active", "BBBB", scm = 2000L)),
    advance_after = 2L))
r <- systemd_restart("cups.service", scope = "user", timeout = 5)
rsystemd:::set_runner(old)
fs$restore()
expect_equal(r$audit_scope, "user")

# --- live: disposable user-scope unit, real systemctl + durable audit ----
# at_home only, and only where a user systemd manager is reachable.
user_ok <- nzchar(Sys.which("systemctl")) &&
    identical(0L, tryCatch(system2("systemctl",
        c("--user", "show-environment"), stdout = FALSE, stderr = FALSE),
        error = function(e) 1L))
if (at_home() && user_ok && nzchar(Sys.which("systemd-run"))) {
    xdgtmp <- tempfile("xdg-")
    dir.create(xdgtmp)
    old_xdg <- Sys.getenv("XDG_STATE_HOME", unset = NA)
    Sys.setenv(XDG_STATE_HOME = xdgtmp)
    keep <- rsystemd:::set_audit_resolver(NULL)   # use the real runix resolver
    unit <- paste0("runix-live-", Sys.getpid(), ".service")
    system2("systemd-run", c("--user", "--collect", paste0("--unit=", unit),
        "sleep", "600"), stdout = FALSE, stderr = FALSE)
    Sys.sleep(0.4)

    # happy path: real restart, durable caller-owned audit
    r <- tryCatch(systemd_restart(unit, scope = "user", timeout = 25),
        error = identity)
    if (!inherits(r, "condition")) {
        expect_equal(r$operation, "systemd.restart")
        expect_equal(r$audit_scope, "user")
        expect_true(r$audit_persisted)
        sinkfile <- file.path(xdgtmp, "runix", "audit.jsonl")
        expect_true(file.exists(sinkfile))
        expect_true(length(readLines(sinkfile, warn = FALSE)) >= 2L)
    }

    # error path: a nonexistent unit yields a typed error carrying a cid
    e <- tryCatch(systemd_restart(paste0("runix-nope-", Sys.getpid(),
        ".service"), scope = "user", timeout = 10), error = identity)
    expect_inherits(e, "rsystemd_error")
    expect_false(is.null(e$correlation_id))

    # cleanup
    system2("systemctl", c("--user", "stop", unit), stdout = FALSE,
        stderr = FALSE)
    system2("systemctl", c("--user", "reset-failed", unit),
        stdout = FALSE, stderr = FALSE)
    if (is.na(old_xdg)) Sys.unsetenv("XDG_STATE_HOME") else {
        Sys.setenv(XDG_STATE_HOME = old_xdg)
    }
    rsystemd:::set_audit_resolver(keep)
}

# Restore the default resolver for any later test file in the session.
rsystemd:::set_audit_resolver(NULL)
