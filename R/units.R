#' List systemd units
#'
#' Queries the system manager for all loaded units (including inactive ones)
#' via systemctl's JSON machine output.
#'
#' @param pattern Optional single glob pattern (e.g. \code{"ssh*"}) to filter
#'   unit names server-side.
#' @return A data.frame with columns \code{unit}, \code{load_state},
#'   \code{active_state}, \code{sub_state}, and \code{description}. Zero rows
#'   when the pattern matches nothing.
#' @examples
#' \dontrun{
#' units <- systemd_units()
#' units[units$active_state == "failed", ]
#' }
#' @export
systemd_units <- function(pattern = NULL) {
    args <- c("list-units", "--all", "--output=json", "--no-pager")
    if (!is.null(pattern)) {
        if (!is.character(pattern) || length(pattern) != 1L || is.na(pattern)) {
            stop_rsystemd("pattern must be a single character string")
        }
        args <- c(args, shQuote(pattern))
    }
    res <- runner()("systemctl", args)
    if (res$status != 0L) {
        stop_rsystemd("systemctl list-units failed with status ", res$status)
    }
    parse_units_json(paste(res$output, collapse = "\n"))
}

## Pure parser, separated from the runner so fixture tests exercise it
## offline. Fail-closed: invalid JSON or missing fields is an error.
parse_units_json <- function(txt) {
    empty <- data.frame(unit = character(), load_state = character(),
                        active_state = character(), sub_state = character(),
                        description = character(), stringsAsFactors = FALSE)
    if (!nzchar(trimws(txt))) {
        return(empty)
    }
    dat <- tryCatch(
                    jsonlite::fromJSON(txt),
                    error = function(e) {
        stop_rsystemd("unparseable systemctl JSON output: ",
                      conditionMessage(e),
                      class = "runix_parse_error")
    }
    )
    if (length(dat) == 0L) {
        return(empty)
    }
    need <- c("unit", "load", "active", "sub", "description")
    missing <- setdiff(need, names(dat))
    if (length(missing) > 0L) {
        stop_rsystemd("systemctl JSON output missing field(s): ",
                      paste(missing, collapse = ", "),
                      class = "runix_parse_error")
    }
    data.frame(
               unit = dat$unit, load_state = dat$load,
               active_state = dat$active, sub_state = dat$sub,
               description = dat$description,
               stringsAsFactors = FALSE
    )
}
