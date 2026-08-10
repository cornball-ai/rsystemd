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
## offline. Fail-closed: invalid JSON, a non-object row, or a missing/wrong-typed
## field is an error (strict per-field validation in R/parse_json.R).
parse_units_json <- function(txt) {
    if (!nzchar(trimws(txt))) {
        return(data.frame(unit = character(), load_state = character(),
                          active_state = character(), sub_state = character(),
                          description = character(), stringsAsFactors = FALSE))
    }
    cols <- .json_columns(txt, list(
                                    list(key = "unit", type = "character"),
                                    list(key = "load", type = "character"),
                                    list(key = "active", type = "character"),
                                    list(key = "sub", type = "character"),
                                    list(key = "description", type = "character")),
                          "systemctl list-units")
    data.frame(unit = cols$unit, load_state = cols$load,
               active_state = cols$active, sub_state = cols$sub,
               description = cols$description, stringsAsFactors = FALSE)
}
