## Negative authorization: the canary principal may manage ONLY
## runix-canary.service. Any other unit falls through the polkit rule to a hard
## deny, and rsystemd surfaces a typed runix_unauthorized rather than mutating.
library(rsystemd)
e <- tryCatch(systemd_restart("systemd-journald.service", timeout = 10),
              error = identity)
stopifnot(inherits(e, "runix_unauthorized"))
cat("DENIED OK: unrelated unit refused with runix_unauthorized\n")
