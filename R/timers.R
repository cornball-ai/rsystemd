#' List systemd timers
#'
#' All timers (including inactive), with their next and last trigger times
#' and the unit each activates. Aggregation: the trigger times come from
#' systemctl's list-timers JSON (usec epoch), the timer's own
#' \code{active_state} is joined from \code{\link{systemd_units}}.
#'
#' @return A data.frame with columns \code{timer}, \code{next_elapse}
#'   (POSIXct UTC, NA when not scheduled), \code{last_trigger} (POSIXct
#'   UTC, NA when never triggered), \code{activates}, and
#'   \code{active_state}.
#' @examples
#' \dontrun{
#' systemd_timers()
#' }
#' @export
systemd_timers <- function() {
    res <- runner()("systemctl",
        c("list-timers", "--all", "--output=json", "--no-pager"))
    if (res$status != 0L) {
        stop_rsystemd("systemctl list-timers failed with status ", res$status)
    }
    timers <- parse_timers_json(paste(res$output, collapse = "\n"))
    if (nrow(timers) == 0L) {
        return(timers)
    }
    units <- systemd_units()
    timers$active_state <- units$active_state[match(timers$timer, units$unit)]
    timers
}

## Pure parser for systemctl list-timers JSON. next/last are usec-epoch
## numbers; JSON null means NA (not scheduled / never triggered). Strict
## per-field validation lives in R/parse_json.R.
parse_timers_json <- function(txt) {
    if (!nzchar(trimws(txt))) {
        return(data.frame(timer = character(),
                          next_elapse = as.POSIXct(character(), tz = "UTC"),
                          last_trigger = as.POSIXct(character(), tz = "UTC"),
                          activates = character(), active_state = character(),
                          stringsAsFactors = FALSE))
    }
    cols <- .json_columns(txt, list(
                                    list(key = "unit", type = "character"),
                                    list(key = "next", type = "number", nullable = TRUE),
                                    list(key = "last", type = "number", nullable = TRUE),
                                    list(key = "activates", type = "character")),
                          "systemctl list-timers")
    usec_time <- function(x) {
        as.POSIXct(as.numeric(x) / 1e6, origin = "1970-01-01", tz = "UTC")
    }
    data.frame(timer = cols$unit,
               next_elapse = usec_time(cols[["next"]]),
               last_trigger = usec_time(cols$last),
               activates = cols$activates,
               active_state = rep(NA_character_, length(cols$unit)),
               stringsAsFactors = FALSE)
}
