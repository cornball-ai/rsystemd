## Typed conditions. rsystemd_error inherits runix_error so callers can catch
## either the package class or the framework class.
stop_rsystemd <- function(..., class = character(), call. = sys.call(-1)) {
    stop(structure(
                   class = c(class, "rsystemd_error", "runix_error", "error",
                             "condition"),
                   list(message = paste0(...), call = call.)
        ))
}

## Mutation errors carry structured data fields (resource, observed,
## elapsed, completion, ...) on the condition object so a caller — and the
## rctl envelope — can branch on class and read the post-state without
## parsing the message. `data` is a named list merged into the condition.
stop_mutation <- function(message, class, data = list(), call. = sys.call(-1)) {
    cond <- c(list(message = message, call = call.), data)
    stop(structure(cond,
                   class = c(class, "rsystemd_error", "runix_error", "error",
                             "condition")))
}
