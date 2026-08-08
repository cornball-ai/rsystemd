## systemd mutation verbs, per the Phase 2 contract
## (cornball-ai/runix docs/phase2-mutation-contract.md). Functional core →
## imperative boundary: the read side reuses systemd_unit_info(); the effect
## is one --no-block systemctl call; the wait is an interruptible R poll
## correlated by InvocationID; the return is a systemd_result.

## ---- shared helpers ---------------------------------------------------

## Observed subset used for before/after and correlation. Scope-aware: a
## user-scope mutation must read the user manager, or InvocationID is always
## NA and correlation silently fails (found in live smoke).
observe_unit <- function(unit, scope = "system") {
    info <- systemd_unit_info(unit, scope = scope)
    info[c("active_state", "sub_state", "unit_file_state", "main_pid",
            "invocation_id", "state_change_monotonic")]
}

## Observe without throwing — for error payloads that must stay truthful
## even when the post-read itself fails. Returns list(observed,
## observed_failed, observed_reason).
safe_observe <- function(unit, scope = "system") {
    tryCatch(
             list(observed = observe_unit(unit, scope), observed_failed = FALSE,
                  observed_reason = NA_character_),
             error = function(e) list(observed = NA, observed_failed = TRUE,
                                      observed_reason = conditionMessage(e))
    )
}

check_unit <- function(unit) {
    if (!is.character(unit) || length(unit) != 1L || is.na(unit) ||
        !nzchar(unit)) {
        stop_rsystemd("unit must be a single unit name")
    }
}

check_scope <- function(scope) {
    if (!identical(scope, "system") && !identical(scope, "user")) {
        stop_rsystemd("scope must be \"system\" or \"user\"")
    }
}

check_timeout <- function(timeout) {
    if (!is.numeric(timeout) || length(timeout) != 1L || is.na(timeout) ||
        timeout <= 0) {
        stop_rsystemd("timeout must be a single positive number of seconds")
    }
}

check_flag <- function(x, name) {
    if (!is.logical(x) || length(x) != 1L || is.na(x)) {
        stop_rsystemd(name, " must be a single TRUE or FALSE")
    }
}

scope_args <- function(scope) if (identical(scope, "user")) "--user" else NULL

## Issue an effect, classifying a non-zero result. stderr (captured by the
## runner) distinguishes a polkit denial from a plain operation failure.
issue_effect <- function(argv, unit, operation, scope = "system") {
    res <- runner()("systemctl", argv)
    if (res$status != 0L) {
        msg <- paste(c(res$output, res$stderr), collapse = " ")
        if (grepl("authenticat|permission|access denied|not authorized|interactive",
                  msg, ignore.case = TRUE)) {
            so <- safe_observe(unit, scope)
            stop_mutation(
                          paste0("authorization denied for ", operation, " ", unit),
                          "runix_unauthorized",
                          data = list(resource = unit,
                                      polkit_action = "org.freedesktop.systemd1.manage-units",
                                      observed = so$observed,
                                      observed_failed = so$observed_failed,
                                      observed_reason = so$observed_reason))
        }
        so <- safe_observe(unit, scope)
        stop_mutation(
                      paste0("systemctl ", operation, " failed for ", unit,
                             " (status ", res$status, ")"),
                      "runix_operation_failed",
                      data = list(resource = unit, observed = so$observed,
                                  observed_failed = so$observed_failed,
                                  observed_reason = so$observed_reason))
    }
    invisible(TRUE)
}

## Interruptible poll. done_fn(after) decides when the state has settled.
## Returns list(status = "done"|"timeout"|"cancelled", after = <observed>).
## Uses proc.time() elapsed for the deadline (not wall clock) and systemd's
## own monotonic marker for correlation inside done_fn.
poll_until <- function(unit, done_fn, timeout, scope = "system") {
    interval <- getOption("rsystemd.poll_interval", 0.2)
    deadline <- proc.time()[["elapsed"]] + timeout
    slp <- sleeper()
    repeat {
        after <- observe_unit(unit, scope)
        if (isTRUE(done_fn(after))) {
            return(list(status = "done", after = after))
        }
        if (proc.time()[["elapsed"]] >= deadline) {
            return(list(status = "timeout", after = after))
        }
        cancelled <- FALSE
        tryCatch(slp(interval), interrupt = function(e) cancelled <<- TRUE,
                 rsystemd_interrupt = function(e) cancelled <<- TRUE)
        if (cancelled) {
            return(list(status = "cancelled",
                        after = observe_unit(unit, scope)))
        }
    }
}

## Build a runix_timeout / runix_cancelled error carrying the observed
## post-state, per the contract's "never leave a lie" rule.
stop_wait <- function(class, operation, unit, elapsed, after) {
    stop_mutation(
                  paste0(operation, " ", unit, ": ",
            if (identical(class, "runix_timeout")) "timed out" else "cancelled",
                         " after ", round(elapsed, 2), "s"),
                  class,
                  data = list(resource = unit, elapsed = elapsed, observed = after,
                              observed_failed = FALSE, observed_reason = NA_character_))
}

## ---- start / stop / restart (queued jobs, correlated) -----------------

## Shared engine for the three active-state verbs. `skip_fn(before)` is TRUE
## only for a pure no-op (no effect issued at all); `functional_pred(before)`
## predicts whether an issued effect would be a *functional* change (for the
## preview `changed`). These are distinct: stopping a `failed` unit is not a
## no-op (it clears the failure, so state_changed) yet is not a functional
## stop (changed = FALSE).
mutate_active <- function(operation, subcommand, unit, scope, dry_run,
                          timeout, skip_fn, functional_pred, done_fn,
                          require_invocation_change, interpret) {
    check_unit(unit)
    check_scope(scope)
    check_flag(dry_run, "dry_run")
    check_timeout(timeout)

    before <- observe_unit(unit, scope)
    would_issue <- !skip_fn(before)

    if (dry_run) {
        return(new_systemd_result(
                                  operation = operation, resource = unit,
                                  changed = functional_pred(before),
                                  state_changed = would_issue,
                                  preview = TRUE, before = before, after = NA,
                                  planned = list(subcommand = subcommand, scope = scope,
                    effect_would_issue = would_issue),
                                  completion = list(method = "preview", job_result = NA_character_,
                    invocation_before = before$invocation_id,
                    invocation_after = NA_character_),
                                  authorized_via = authz_for(operation, scope, FALSE)))
    }

    ## Pure no-op: already cleanly in the desired state, no effect issued.
    if (!would_issue) {
        return(new_systemd_result(
                                  operation = operation, resource = unit, changed = FALSE,
                                  state_changed = FALSE, preview = FALSE, before = before,
                                  after = before,
                                  planned = list(subcommand = subcommand, scope = scope,
                    effect_would_issue = FALSE),
                                  completion = list(method = "noop", job_result = NA_character_,
                    invocation_before = before$invocation_id,
                    invocation_after = before$invocation_id),
                                  authorized_via = authz_for(operation, scope, FALSE)))
    }

    started <- proc.time()[["elapsed"]]
    argv <- c(scope_args(scope), subcommand, "--no-block", shQuote(unit))
    issue_effect(argv, unit, operation, scope)

    poll <- poll_until(unit, function(after) done_fn(after, before), timeout,
                       scope)
    elapsed <- proc.time()[["elapsed"]] - started
    after <- poll$after

    if (identical(poll$status, "cancelled")) {
        stop_wait("runix_cancelled", operation, unit, elapsed, after)
    }
    if (identical(poll$status, "timeout")) {
        ## restart whose invocation never advanced = submitted, not a lie.
        if (require_invocation_change &&
            identical(after$invocation_id, before$invocation_id)) {
            return(new_systemd_result(
                                      operation = operation, resource = unit, changed = NA,
                                      state_changed = NA, preview = FALSE, before = before,
                                      after = after,
                                      planned = list(subcommand = subcommand, scope = scope),
                                      completion = list(method = "submitted_unconfirmed",
                        job_result = NA_character_,
                        invocation_before = before$invocation_id,
                        invocation_after = after$invocation_id),
                                      authorized_via = authz_for(operation, scope, TRUE),
                                      outcome = "submitted"))
        }
        stop_wait("runix_timeout", operation, unit, elapsed, after)
    }

    ## Settled: let the verb interpret success vs failure. An effect was
    ## issued, so the authorization descriptor reflects that.
    interpret(before, after, operation, unit, authz_for(operation, scope, TRUE))
}

#' Start a systemd unit
#'
#' @param unit A single unit name.
#' @param scope \code{"system"} (default) or \code{"user"}.
#' @param dry_run Preview only; issue no effect.
#' @param timeout Seconds to wait for the job to settle (default 90).
#' @return A \code{systemd_result}.
#' @export
systemd_start <- function(unit, scope = "system", dry_run = FALSE,
                          timeout = 90) {
    mutate_active("systemd.start", "start", unit, scope, dry_run, timeout,
                  skip_fn = function(b) identical(b$active_state, "active"),
                  functional_pred = function(b) {
        !identical(b$active_state, "active")
    },
                  done_fn = function(after, before) {
        after$active_state %in% c("active", "failed")
    },
                  require_invocation_change = FALSE,
                  interpret = function(before, after, operation, unit, az) {
        if (identical(after$active_state, "failed")) {
            stop_mutation(paste0(unit, " failed to start"),
                          "runix_operation_failed",
                          data = list(resource = unit, observed = after,
                                      observed_failed = FALSE,
                                      observed_reason = NA_character_))
        }
        finish_result(operation, unit, before, after, "state_transition", az)
    })
}

#' Stop a systemd unit
#' @inheritParams systemd_start
#' @return A \code{systemd_result}.
#' @export
systemd_stop <- function(unit, scope = "system", dry_run = FALSE,
                         timeout = 90) {
    mutate_active("systemd.stop", "stop", unit, scope, dry_run, timeout,
                  skip_fn = function(b) identical(b$active_state, "inactive"),
                  functional_pred = function(b) {
        b$active_state %in% c("active", "activating", "reloading")
    },
                  done_fn = function(after, before) {
        after$active_state %in% c("inactive", "failed")
    },
                  require_invocation_change = FALSE,
                  interpret = function(before, after, operation, unit, az) {
        finish_result(operation, unit, before, after, "state_transition", az)
    })
}

#' Restart a systemd unit
#'
#' Always issues the effect (a restart is never idempotent) and confirms a
#' fresh invocation ran before reporting success.
#' @inheritParams systemd_start
#' @return A \code{systemd_result}.
#' @export
systemd_restart <- function(unit, scope = "system", dry_run = FALSE,
                            timeout = 90) {
    mutate_active("systemd.restart", "restart", unit, scope, dry_run,
                  timeout,
                  skip_fn = function(b) FALSE, # never idempotent
                  functional_pred = function(b) TRUE, # always acts
                  done_fn = function(after, before) {
        invocation_changed(after, before) &&
        after$active_state %in% c("active", "failed", "inactive")
    },
                  require_invocation_change = TRUE,
                  interpret = function(before, after, operation, unit, az) {
        if (identical(after$active_state, "failed")) {
            stop_mutation(paste0(unit, " failed to restart"),
                          "runix_operation_failed",
                          data = list(resource = unit, observed = after,
                                      observed_failed = FALSE,
                                      observed_reason = NA_character_))
        }
        finish_result(operation, unit, before, after, "invocation_id", az)
    })
}

invocation_changed <- function(after, before) {
    !is.na(after$invocation_id) &&
    !identical(after$invocation_id, before$invocation_id)
}

## Build the success result for a settled active-state mutation. `changed`
## is the functional effect; state_changed is any observed field difference.
finish_result <- function(operation, unit, before, after, method,
                          authorized_via) {
    fields <- c("active_state", "sub_state", "unit_file_state", "main_pid",
                "invocation_id", "state_change_monotonic")
    state_changed <- !identical(before[fields], after[fields])
    changed <- functional_changed(operation, before, after)
    new_systemd_result(
                       operation = operation, resource = unit, changed = changed,
                       state_changed = state_changed, preview = FALSE, before = before,
                       after = after,
                       planned = list(already_in_desired_state = FALSE),
                       completion = list(method = method,
            job_result = if (identical(after$active_state, "failed")) {
                "failed"
            } else {
                "done"
            },
            invocation_before = before$invocation_id,
            invocation_after = after$invocation_id),
                       authorized_via = authorized_via)
}

## Verb-specific functional-change semantics (distinct from state_changed).
functional_changed <- function(operation, before, after) {
    running <- c("active", "activating", "reloading")
    switch(operation,
           "systemd.start" = !(before$active_state %in% "active") &&
           identical(after$active_state, "active"),
           "systemd.stop" = (before$active_state %in% running) &&
           after$active_state %in% c("inactive", "failed"),
           "systemd.restart" = invocation_changed(after, before) &&
           !identical(after$active_state, "failed"),
           NA)
}

## ---- enable / disable (synchronous unit-file operations) --------------

mutate_unit_file <- function(operation, subcommand, unit, scope, dry_run,
                             timeout, desired_set) {
    check_unit(unit)
    check_scope(scope)
    check_flag(dry_run, "dry_run")
    check_timeout(timeout)

    before <- observe_unit(unit, scope)
    would_change <- !(before$unit_file_state %in% desired_set)

    if (dry_run) {
        return(new_systemd_result(operation, unit, would_change,
                                  would_change, TRUE, before, NA,
                                  planned = list(subcommand = subcommand, scope = scope,
                    already_in_desired_state = !would_change),
                                  completion = list(method = "preview", job_result = NA_character_,
                    invocation_before = before$invocation_id,
                    invocation_after = NA_character_),
                                  authorized_via = authz_for(operation, scope, FALSE)))
    }

    if (!would_change) {
        return(new_systemd_result(operation, unit, FALSE, FALSE, FALSE,
                                  before, before,
                                  planned = list(subcommand = subcommand, scope = scope,
                    already_in_desired_state = TRUE),
                                  completion = list(method = "noop", job_result = NA_character_,
                    invocation_before = before$invocation_id,
                    invocation_after = before$invocation_id),
                                  authorized_via = authz_for(operation, scope, FALSE)))
    }

    argv <- c(scope_args(scope), subcommand, shQuote(unit))
    issue_effect(argv, unit, operation, scope) # synchronous; no --no-block
    after <- observe_unit(unit, scope)
    if (!(after$unit_file_state %in% desired_set)) {
        stop_mutation(
                      paste0(operation, " did not take effect for ", unit,
                             " (unit_file_state=", after$unit_file_state, ")"),
                      "runix_operation_failed",
                      data = list(resource = unit, observed = after,
                                  observed_failed = FALSE, observed_reason = NA_character_))
    }
    fields <- c("active_state", "sub_state", "unit_file_state", "main_pid",
                "invocation_id", "state_change_monotonic")
    new_systemd_result(operation, unit, changed = TRUE,
                       state_changed = !identical(before[fields], after[fields]),
                       preview = FALSE, before = before, after = after,
                       planned = list(already_in_desired_state = FALSE),
                       completion = list(method = "synchronous", job_result = "done",
            invocation_before = before$invocation_id,
            invocation_after = after$invocation_id),
                       authorized_via = authz_for(operation, scope, TRUE))
}

#' Enable a systemd unit
#' @inheritParams systemd_start
#' @return A \code{systemd_result}.
#' @export
systemd_enable <- function(unit, scope = "system", dry_run = FALSE,
                           timeout = 90) {
    mutate_unit_file("systemd.enable", "enable", unit, scope, dry_run,
                     timeout, desired_set = c("enabled", "enabled-runtime"))
}

#' Disable a systemd unit
#' @inheritParams systemd_start
#' @return A \code{systemd_result}.
#' @export
systemd_disable <- function(unit, scope = "system", dry_run = FALSE,
                            timeout = 90) {
    mutate_unit_file("systemd.disable", "disable", unit, scope, dry_run,
                     timeout, desired_set = c("disabled", "masked"))
}
