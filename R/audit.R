## Durable-audit persistence for mutations (runix durable-audit-contract.md).
## The verbs build the in-memory result/condition as before; this layer routes
## each attempt through the runix core sink: one record for a non-effect path
## (preview / pre-effect no-op), and a two-phase intent+outcome pair for an
## effect, all sharing one correlation_id. The sink is resolved by scope and
## caller privilege (caller-owned vs system) and is injectable for tests.

.audit_state <- new.env(parent = emptyenv())

## Resolver: function(scope) -> list(sink, audit_scope, system_durable_audit).
## Default asks the runix core for the scope-appropriate sink (fsync
## durability); tests inject an in-memory resolver via set_audit_resolver().
default_audit_resolver <- function(scope) {
    runix::default_audit_sink(scope, durability = "fsync")
}

audit_resolver <- function() {
    f <- .audit_state$resolver
    if (is.null(f)) {
        default_audit_resolver
    } else {
        f
    }
}

set_audit_resolver <- function(f = NULL) {
    old <- .audit_state$resolver
    .audit_state$resolver <- f
    invisible(old)
}

## Stamp the audit outcome onto a result so a caller sees how and where the
## attempt was recorded.
stamp_result <- function(result, cid, audit_scope, audit_persisted) {
    result$correlation_id <- cid
    result$audit_scope <- audit_scope
    result$audit_persisted <- audit_persisted
    result
}

## Domain content of the intent record: the planned operation, before any
## effect is issued.
intent_record <- function(operation, unit, scope, audit_scope) {
    list(operation = operation, resource = unit, scope = scope,
         preview = FALSE, effect_issued = FALSE, actor = actor_id(),
         audit_scope = audit_scope, outcome = "intent")
}

## Domain content of an outcome / non-effect record built from a systemd_result.
## `time` is intentionally omitted: the core framing (.finish_record) stamps
## it, and a duplicate key would be rejected by the encoder.
audit_record_from_result <- function(result, effect_issued, audit_scope,
                                     scope) {
    a <- result$audit
    list(operation = a$operation, resource = a$resource, scope = scope,
         preview = a$preview, effect_issued = effect_issued,
         changed = a$changed, state_changed = a$state_changed,
         actor = a$actor, authorized_via = a$authorized_via,
         completion_method = a$completion_method, job_result = a$job_result,
         observed = result$after, audit_scope = audit_scope,
         outcome = a$outcome)
}

## Domain content of an outcome record built from a typed mutation condition
## (an effect was issued, then failed/timed out/was denied). The observed
## post-state and elapsed carried on the condition are preserved.
audit_record_from_condition <- function(cond, operation, unit, scope,
                                        audit_scope) {
    resource <- if (is.null(cond$resource)) unit else cond$resource
    list(operation = operation, resource = resource, scope = scope,
         preview = FALSE, effect_issued = TRUE, actor = actor_id(),
         observed = cond$observed,
         observed_failed = if (is.null(cond$observed_failed)) {
             NA
         } else {
             cond$observed_failed
         },
         elapsed = cond$elapsed, audit_scope = audit_scope,
         outcome = outcome_from_condition(cond))
}

## Map a typed mutation condition to its audit outcome label.
outcome_from_condition <- function(cond) {
    cl <- class(cond)
    if ("runix_unauthorized" %in% cl) {
        "unauthorized"
    } else if ("runix_timeout" %in% cl) {
        "timeout"
    } else if ("runix_cancelled" %in% cl) {
        "cancelled"
    } else if ("runix_operation_failed" %in% cl) {
        "failed"
    } else {
        "error"
    }
}

## Emit the single record for a non-effect path (preview / no-op) and stamp
## the result.
audit_noneffect <- function(result, resolved, cid, phase, scope) {
    rec <- audit_record_from_result(result, effect_issued = FALSE,
                                    resolved$audit_scope, scope)
    e <- runix::audit_emit(resolved$sink, rec, phase = phase,
                           correlation_id = cid)
    stamp_result(result, cid, resolved$audit_scope, isTRUE(e$persisted))
}

## Run the effect section under the two-phase discipline: durable intent
## before the effect is issued, outcome after, one correlation_id, typed
## failures recorded richly and re-raised with the id. `run` is the effect
## thunk (issue + poll + interpret) returning a systemd_result or raising a
## typed condition.
audit_effect <- function(operation, unit, scope, resolved, cid, run) {
    tp <- runix::audit_two_phase(
                                 resolved$sink,
                                 intent = intent_record(operation, unit, scope,
                                                        resolved$audit_scope),
                                 effect = function(.cid) run(),
                                 outcome = function(res) {
                                     audit_record_from_result(res, effect_issued = TRUE,
                                                              resolved$audit_scope, scope)
                                 },
                                 on_error = function(cond, .cid) {
                                     audit_record_from_condition(cond, operation, unit, scope,
                                                                 resolved$audit_scope)
                                 },
                                 id_fn = function() cid)
    stamp_result(tp$result, cid, resolved$audit_scope, tp$audit_persisted)
}
