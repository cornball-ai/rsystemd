## rsystemd broker-integration, mutation half. Runs as the unprivileged
## `canary` user against the root, socket-activated broker daemon. Proves an
## unprivileged system-scope restart obtains a SYSTEM-durable audit record it
## could not write itself, with the actor DERIVED from the caller's kernel
## credentials -- never supplied in the record. This is the exact A1-canary
## production path that was fail-closed broken when rsystemd put `actor` in the
## record and the broker rejected it schema_invalid.
library(rsystemd)
uid <- as.integer(Sys.getenv("CANARY_UID", "2001"))

r <- systemd_restart("runix-canary.service", timeout = 15)

stopifnot(
    isTRUE(r$audit_persisted),                       # durable
    identical(r$audit_scope, "system"),              # via the broker, system-scope
    identical(r$audit$actor, sprintf("uid:%d", uid)),# sink-derived actor
    identical(r$completion$method, "invocation_id"), # confirmed by a fresh job
    is.character(r$correlation_id) && nzchar(r$correlation_id),
    !identical(r$completion$invocation_before, r$completion$invocation_after)
)
writeLines(r$correlation_id, "/tmp/rsys-cid.txt")
cat("MUTATE OK: unprivileged system-durable restart via broker; actor uid:",
    uid, "cid", r$correlation_id, "\n")
