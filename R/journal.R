#' Read systemd journal entries
#'
#' Queries the journal via journalctl's JSON machine output and returns the
#' newest entries, oldest first (newest last).
#'
#' @param unit Optional single unit name or glob to filter on.
#' @param priority Optional single integer 0-7: keep entries at that
#'   severity or more severe (journalctl semantics).
#' @param since,until Optional single timestamp strings, passed to
#'   journalctl verbatim (it accepts e.g. "2026-08-07 12:00:00",
#'   "yesterday", "-2h").
#' @param n Maximum number of entries to return (most recent), default 1000.
#' @return A data.frame with columns \code{time} (POSIXct, UTC),
#'   \code{priority} (integer 0-7), \code{unit}, \code{pid} (integer), and
#'   \code{message}. Fields a journal entry does not carry (kernel messages
#'   have no unit, for example) are \code{NA}. What is visible depends on
#'   the caller's journal permissions (adm/systemd-journal group membership
#'   for the full journal).
#' @examples
#' \dontrun{
#' logs <- systemd_journal(priority = 3L, n = 100L)
#' logs[, c("time", "unit", "message")]
#' }
#' @export
systemd_journal <- function(unit = NULL, priority = NULL, since = NULL,
                            until = NULL, n = 1000L) {
    if (!is.numeric(n) || length(n) != 1L || is.na(n) || n < 1L) {
        stop_rsystemd("n must be a single positive number")
    }
    args <- c("-o", "json", "--no-pager", paste0("--lines=", as.integer(n)))
    if (!is.null(priority)) {
        if (!is.numeric(priority) || length(priority) != 1L ||
            is.na(priority) || !priority %in% 0:7) {
            stop_rsystemd("priority must be a single integer between 0 and 7")
        }
        args <- c(args, paste0("--priority=", as.integer(priority)))
    }
    one_string <- function(x, what) {
        if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
            stop_rsystemd(what, " must be a single non-empty string")
        }
        x
    }
    if (!is.null(unit)) {
        args <- c(args, "--unit", shQuote(one_string(unit, "unit")))
    }
    if (!is.null(since)) {
        args <- c(args, paste0("--since=", shQuote(one_string(since, "since"))))
    }
    if (!is.null(until)) {
        args <- c(args, paste0("--until=", shQuote(one_string(until, "until"))))
    }
    res <- runner()("journalctl", args)
    if (res$status != 0L) {
        stop_rsystemd("journalctl failed with status ", res$status)
    }
    parse_journal_json(res$output)
}

## Pure parser for journalctl -o json (one JSON object per line).
## Fail-closed on invalid JSON, missing timestamps, or non-numeric
## numeric fields; missing optional fields (unit, pid, message) are NA.
## Journal fields can arrive as byte arrays when not valid UTF-8; those
## are converted to character.
parse_journal_json <- function(lines) {
    lines <- lines[nzchar(lines)]
    empty <- data.frame(time = as.POSIXct(character(), tz = "UTC"),
                        priority = integer(), unit = character(),
                        pid = integer(), message = character(),
                        stringsAsFactors = FALSE)
    if (length(lines) == 0L) {
        return(empty)
    }
    recs <- vector("list", length(lines))
    for (i in seq_along(lines)) {
        recs[[i]] <- tryCatch(
                              jsonlite::fromJSON(lines[i]),
                              error = function(e) {
            stop_rsystemd("unparseable journal JSON (line ", i, "): ",
                          conditionMessage(e),
                          class = "runix_parse_error")
        }
        )
    }
    chr1 <- function(r, k) {
        v <- r[[k]]
        if (is.null(v)) {
            NA_character_
        } else if (is.character(v)) {
            v[1L]
        } else if (is.numeric(v)) {
            rawToChar(as.raw(v))
        } else {
            NA_character_
        }
    }
    int_strict <- function(x, what) {
        out <- suppressWarnings(as.integer(x))
        bad <- !is.na(x) & is.na(out)
        if (any(bad)) {
            stop_rsystemd("non-numeric ", what, " in journal entry ",
                          which(bad)[1L], ": ", x[which(bad)[1L]],
                          class = "runix_parse_error")
        }
        out
    }
    ts <- vapply(recs, chr1, character(1), "__REALTIME_TIMESTAMP")
    if (anyNA(ts)) {
        stop_rsystemd("journal entry ", which(is.na(ts))[1L],
                      " missing __REALTIME_TIMESTAMP",
                      class = "runix_parse_error")
    }
    data.frame(
               time = as.POSIXct(as.numeric(ts) / 1e6, origin = "1970-01-01",
                                 tz = "UTC"),
               priority = int_strict(vapply(recs, chr1, character(1), "PRIORITY"),
                                     "PRIORITY"),
               unit = vapply(recs, chr1, character(1), "_SYSTEMD_UNIT"),
               pid = int_strict(vapply(recs, chr1, character(1), "_PID"), "_PID"),
               message = vapply(recs, chr1, character(1), "MESSAGE"),
               stringsAsFactors = FALSE
    )
}
