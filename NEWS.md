# rsystemd 0.0.1.9

Normalize the caller identity in mutation results and audit records to the
shared `uid:<numeric uid>` form (durable-audit-contract.md), replacing the
previous `name(uid)` display string. The numeric uid is authoritative and now
matches what every Runix sink (including the audit broker) records. This is a
user-facing change to `result$audit$actor` and the persisted audit `actor`.

# rsystemd 0.0.1.8

Adopt the runix receipt-based sink interface (`open_intent`/`write_outcome`):
the correlation id is now minted by the sink rather than by the verb, so the
same code path works with a future remote broker sink. Internal refactor, no
change to observable mutation or audit behavior.

# rsystemd 0.0.1.7

## Behavior change: mutations now emit durable audit records

Every mutation attempt (`systemd_start`/`stop`/`restart`/`enable`/`disable`)
is now persisted through the `runix` durable-audit sink, per
`durable-audit-contract.md`:

- an effect writes a durable **intent** record *before* `systemctl` is
  submitted (fail-closed: if the intent cannot be persisted, no effect is
  issued), then an **outcome** record after; a preview or a pre-effect no-op
  writes a single non-effect record;
- error paths (timeout, cancellation, failure, unauthorized) now emit an
  outcome record too (previously only successes were auditable in memory);
- results and raised conditions carry a shared `correlation_id`, plus
  `audit_scope` and `audit_persisted`.

Authority follows the ratified authority matrix: root writes the system sink;
otherwise the caller-owned XDG sink is used with `audit_scope = "caller"` (for
system scope) or `"user"`, and `audit_persisted`/`audit_scope` report the
truth. **Autonomous fleet-wide system mutation remains disabled by policy**
until the privileged audit broker exists; this release enables honest
local/manual operation, not broker-grade unattended mutation.

The sink is injectable (`set_audit_resolver`) so tests and embedders can
substitute an in-memory sink.
