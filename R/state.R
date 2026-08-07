#' Overall system manager state
#'
#' The system manager's own verdict plus the failed units behind it.
#' Aggregation: the state word comes from \code{systemctl
#' is-system-running}; \code{failed_units} collects
#' \code{\link{systemd_units}} rows with \code{active_state "failed"}.
#'
#' @return A list with \code{state} (e.g. \code{"running"},
#'   \code{"degraded"}) and \code{failed_units} (character vector, empty
#'   when none).
#' @examples
#' \dontrun{
#' systemd_state()
#' }
#' @export
systemd_state <- function() {
    res <- runner()("systemctl", "is-system-running")
    ## is-system-running exits non-zero whenever the word is not "running";
    ## for this one command the exit code is data, not an error — the word
    ## on stdout is authoritative.
    if (length(res$output) < 1L || !nzchar(trimws(res$output[1L]))) {
        stop_rsystemd("empty is-system-running output",
                      class = "runix_parse_error")
    }
    units <- systemd_units()
    list(
         state = trimws(res$output[1L]),
         failed_units = units$unit[units$active_state == "failed"]
    )
}
