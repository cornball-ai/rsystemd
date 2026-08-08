## Constructor for the mutation result object, per the Phase 2 contract
## (cornball-ai/runix docs/phase2-mutation-contract.md). The neutral shell and
## S3 class c("systemd_result", "runix_result") come from runix::new_runix_result;
## this wrapper adds the systemd-specific audit record. A plain list, data-in/
## data-out.

new_systemd_result <- function(operation, resource, changed, state_changed,
                               preview, before, after, planned, completion,
                               authorized_via = "unknown", outcome = "ok") {
    audit <- new_audit(operation, resource, preview, changed, state_changed,
                       completion, authorized_via = authorized_via,
                       outcome = outcome)
    runix::new_runix_result(operation, resource, changed, state_changed,
                            preview, before, after, planned, completion,
                            audit, subclass = "systemd_result")
}

## The audit record. `actor` is the caller's uid resolved once; failures
## build their own audit via this same helper with the matching outcome.
## authorized_via records how (or whether) the effect was authorized — never
## asserted on previews/no-ops, where no effect was issued (see authz_for).
new_audit <- function(operation, resource, preview, changed, state_changed,
                      completion, authorized_via, outcome) {
    list(operation = operation, resource = resource, preview = preview,
         changed = changed, state_changed = state_changed,
         actor = actor_id(), authorized_via = authorized_via,
         completion_method = completion$method,
         job_result = completion$job_result, time = current_time(),
         outcome = outcome)
}

## Explicit authorization descriptor for the audit record. No effect issued
## (preview or idempotent no-op) => "not_required". A user-scope effect goes
## through the caller's own manager, not a system polkit action =>
## "not_required". A system-scope effect => the polkit action systemd itself
## checks for that verb.
authz_for <- function(operation, scope, effect_issued) {
    if (!effect_issued || identical(scope, "user")) {
        return("not_required")
    }
    action <- switch(operation, "systemd.start" =, "systemd.stop" =,
                     "systemd.restart" = "manage-units", "systemd.enable" =,
                     "systemd.disable" = "manage-unit-files", NULL)
    if (is.null(action)) {
        return("unknown")
    }
    paste0("polkit:org.freedesktop.systemd1.", action)
}

## Caller identity for the audit trail: numeric uid and login name.
actor_id <- function() {
    uid <- tryCatch(as.integer(system2("id", "-u", stdout = TRUE,
                                       stderr = FALSE)[1L]), error = function(e) NA_integer_)
    name <- tryCatch(Sys.info()[["user"]], error = function(e) NA_character_)
    paste0(if (is.na(name)) "?" else name, "(", uid, ")")
}

## Wall-clock stamp for the audit record only (never for poll timing —
## that uses systemd's monotonic marker). Isolated so tests can see it.
current_time <- function() {
    t <- Sys.time()
    attr(t, "tzone") <- "UTC"
    t
}

#' @export
print.systemd_result <- function(x, ...) {
    verb <- sub("^systemd\\.", "", x$operation)
    if (x$preview) {
        tag <- " [preview]"
    } else {
        tag <- ""
    }
    cat(sprintf("%s %s%s: changed=%s state_changed=%s (%s)\n", verb,
                x$resource, tag, x$changed, x$state_changed,
                x$completion$method))
    invisible(x)
}
