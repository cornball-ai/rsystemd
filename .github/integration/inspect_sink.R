## rsystemd broker-integration, inspection half. Runs as root. Confirms the
## unprivileged mutation produced a durable intent+outcome pair in the SYSTEM
## sink, sharing one correlation_id, with the actor and broker.peer identity
## equal to the kernel-verified caller -- never a value the client supplied.
uid <- as.integer(Sys.getenv("CANARY_UID", "2001"))
cid <- readLines("/tmp/rsys-cid.txt", warn = FALSE)[1L]
sink <- "/var/log/runix/audit.jsonl"
stopifnot(file.exists(sink))

recs <- lapply(readLines(sink, warn = FALSE), janssonr::from_json)
mine <- Filter(function(x) identical(x$correlation_id, cid), recs)
phase <- vapply(mine, function(x) {
    if (is.null(x$phase)) NA_character_ else x$phase
}, character(1))
stopifnot("intent" %in% phase, "outcome" %in% phase)

intent <- mine[[which(phase == "intent")[1L]]]
outcome <- mine[[which(phase == "outcome")[1L]]]
want <- sprintf("uid:%d", uid)
stopifnot(
    identical(intent$actor, want),                      # sink-stamped identity
    identical(outcome$actor, want),
    identical(as.integer(intent$broker$peer$uid), uid), # SO_PEERCRED, kernel-verified
    identical(intent$scope, "system"),
    isFALSE(intent$effect_issued),                      # intent: before the effect
    isTRUE(outcome$effect_issued),                      # outcome: effect issued
    identical(outcome$outcome, "ok")
)
cat("INSPECT OK: intent+outcome share cid", cid,
    "; actor and broker.peer.uid == caller", uid, "\n")
