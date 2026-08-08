## Typed conditions built on the runix core taxonomy. Both constructors wrap
## runix::runix_abort() so rsystemd_error inherits runix_error and callers can
## catch either the package class or the framework class.
stop_rsystemd <- function(..., class = character(), call. = sys.call(-1)) {
    cl <- call. # force the default in this frame so the call is the caller's
    runix::runix_abort(paste0(...), subclass = c(class, "rsystemd_error"),
                       call = cl)
}

## Mutation errors carry structured data fields (resource, observed, elapsed,
## completion, ...) on the condition object so a caller -- and the rctl
## envelope -- can branch on class and read the post-state without parsing the
## message. `data` is a named list merged into the condition by runix_abort().
stop_mutation <- function(message, class, data = list(), call. = sys.call(-1)) {
    cl <- call.
    runix::runix_abort(message, subclass = c(class, "rsystemd_error"),
                       data = data, call = cl)
}
