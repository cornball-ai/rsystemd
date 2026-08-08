## Injectable runner: exported functions never call system2() directly.
## Tests replace the runner via set_runner(); set_runner(NULL) restores the
## default. The default enforces LC_ALL=C and fails closed on missing tools —
## an absent systemctl means no systemd, which is an error, never an empty
## result.

.runner_state <- new.env(parent = emptyenv())

runner <- function() {
    run <- .runner_state$run
    if (is.null(run)) {
        run_system
    } else {
        run
    }
}

set_runner <- function(run = NULL) {
    old <- .runner_state$run
    .runner_state$run <- run
    invisible(old)
}

## Injectable sleeper for the mutation poll loop. Default Sys.sleep (which
## is interruptible by SIGINT). Tests replace it to simulate cancellation
## (raise an interrupt-classed condition) or to run instantly.
sleeper <- function() {
    f <- .runner_state$sleep
    if (is.null(f)) {
        Sys.sleep
    } else {
        f
    }
}

set_sleeper <- function(f = NULL) {
    old <- .runner_state$sleep
    .runner_state$sleep <- f
    invisible(old)
}

## On a Unix-alike, system2() concatenates the command and args into one
## command line run via /bin/sh, so an unquoted argument can be interpreted
## as shell syntax. Invariant: every non-literal argument (unit glob
## patterns) is shQuote()d at its call site; the command name is quoted by
## system2() itself.
run_system <- function(cmd, args) {
    if (Sys.which(cmd) == "") {
        stop_rsystemd("backend tool not found: ", cmd,
                      class = "rsystemd_missing_tool")
    }
    ## TZ=UTC makes systemctl show timestamps parse deterministically;
    ## journalctl JSON timestamps are usec-epoch and unaffected. stderr is
    ## captured to a file (kept separate from stdout so parsers see clean
    ## output) — mutations read it to classify authorization failures.
    errfile <- tempfile("rsystemd-stderr")
    on.exit(unlink(errfile), add = TRUE)
    out <- suppressWarnings(
                            system2(cmd, args, stdout = TRUE, stderr = errfile,
                                    env = c("LC_ALL=C", "TZ=UTC"))
    )
    status <- attr(out, "status")
    errlines <- if (file.exists(errfile)) {
        readLines(errfile, warn = FALSE)
    } else {
        character()
    }
    list(
         status = if (is.null(status)) 0L else as.integer(status),
         output = as.character(out),
         stderr = errlines
    )
}
