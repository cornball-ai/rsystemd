## Injectable runner over the runix core (runix::new_runner). Exported
## functions never call system2() directly; tests replace the runner via
## set_runner() and set_runner(NULL) restores the default. The default
## enforces LC_ALL=C and TZ=UTC and fails closed on a missing tool with an
## rsystemd-typed condition -- an absent systemctl means no systemd, which is
## an error, never an empty result.
##
## TZ=UTC makes systemctl show timestamps parse deterministically; journalctl
## JSON timestamps are usec-epoch and unaffected. The default executor
## captures stderr separately from stdout (so parsers see clean output);
## mutations read result$stderr to classify authorization failures.
##
## On a Unix-alike, system2() concatenates the command and args into one
## command line run via /bin/sh, so an unquoted argument can be interpreted as
## shell syntax. Invariant: every non-literal argument (unit glob patterns) is
## shQuote()d at its call site; the command name is quoted by system2() itself.
##
## The injectable sleeper drives the mutation poll loop. Default Sys.sleep
## (interruptible by SIGINT); tests replace it to simulate cancellation (raise
## an interrupt-classed condition) or to run instantly.
.rsystemd_runner <- runix::new_runner(
    default_env = c("LC_ALL=C", "TZ=UTC"),
    missing_tool_subclass = c("rsystemd_missing_tool", "rsystemd_error"))

runner <- .rsystemd_runner$runner
set_runner <- .rsystemd_runner$set_runner
sleeper <- .rsystemd_runner$sleeper
set_sleeper <- .rsystemd_runner$set_sleeper
run_system <- .rsystemd_runner$run_system
