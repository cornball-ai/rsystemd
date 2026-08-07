#' Typed properties of one systemd unit
#'
#' Queries systemctl show for a documented subset of unit properties.
#' Absence of a unit is data here: a not-found unit returns its record with
#' \code{load_state "not-found"}, it is not an error.
#'
#' @param unit A single unit name.
#' @return A named list: \code{unit}, \code{description}, \code{load_state},
#'   \code{active_state}, \code{sub_state}, \code{unit_file_state},
#'   \code{fragment_path}, \code{active_enter_time} (POSIXct UTC, NA when
#'   never active), \code{main_pid} (integer, NA when none),
#'   \code{memory_current} (numeric bytes, NA when not set),
#'   \code{restarts} (integer).
#' @examples
#' \dontrun{
#' info <- systemd_unit_info("ssh.service")
#' info$active_state
#' }
#' @export
systemd_unit_info <- function(unit) {
    if (!is.character(unit) || length(unit) != 1L || is.na(unit) ||
        !nzchar(unit)) {
        stop_rsystemd("unit must be a single unit name")
    }
    props <- c("Id", "Description", "LoadState", "ActiveState", "SubState",
               "UnitFileState", "FragmentPath", "ActiveEnterTimestamp",
               "MainPID", "MemoryCurrent", "NRestarts")
    res <- runner()("systemctl", c("show", shQuote(unit),
                                   paste0("--property=", paste(props, collapse = ",")), "--no-pager"))
    if (res$status != 0L) {
        stop_rsystemd("systemctl show failed with status ", res$status)
    }
    parse_unit_show(res$output)
}

## Pure parser for systemctl show key=value output. The runner forces
## LC_ALL=C and TZ=UTC, so timestamps arrive as
## "%a %Y-%m-%d %H:%M:%S UTC" deterministically.
parse_unit_show <- function(lines) {
    lines <- lines[nzchar(lines)]
    eq <- regexpr("=", lines, fixed = TRUE)
    if (length(lines) == 0L || any(eq < 1L)) {
        stop_rsystemd("unparseable systemctl show output",
                      class = "runix_parse_error")
    }
    kv <- substring(lines, eq + 1L)
    names(kv) <- substr(lines, 1L, eq - 1L)
    if (!all(c("Id", "LoadState") %in% names(kv))) {
        stop_rsystemd("systemctl show output missing Id or LoadState",
                      class = "runix_parse_error")
    }
    val <- function(k) {
        if (k %in% names(kv)) {
            kv[[k]]
        } else {
            NA_character_
        }
    }
    int_strict <- function(x, what) {
        out <- suppressWarnings(as.integer(x))
        if (!is.na(x) && nzchar(x) && is.na(out)) {
            stop_rsystemd("non-numeric ", what, " in show output: ", x,
                          class = "runix_parse_error")
        }
        out
    }
    ts <- val("ActiveEnterTimestamp")
    active_enter_time <- if (is.na(ts) || ts == "" || ts == "n/a") {
        as.POSIXct(NA_character_, tz = "UTC")
    } else {
        out <- as.POSIXct(ts, format = "%a %Y-%m-%d %H:%M:%S UTC", tz = "UTC")
        if (is.na(out)) {
            stop_rsystemd("unparseable timestamp in show output: ", ts,
                          class = "runix_parse_error")
        }
        out
    }
    main_pid <- int_strict(val("MainPID"), "MainPID")
    if (!is.na(main_pid) && main_pid == 0L) {
        main_pid <- NA_integer_
    }
    mc <- val("MemoryCurrent")
    memory_current <- if (is.na(mc) || mc == "" || mc == "[not set]") {
        NA_real_
    } else {
        out <- suppressWarnings(as.numeric(mc))
        if (is.na(out)) {
            stop_rsystemd("non-numeric MemoryCurrent in show output: ", mc,
                          class = "runix_parse_error")
        }
        out
    }
    list(
         unit = kv[["Id"]],
         description = val("Description"),
         load_state = kv[["LoadState"]],
         active_state = val("ActiveState"),
         sub_state = val("SubState"),
         unit_file_state = val("UnitFileState"),
         fragment_path = val("FragmentPath"),
         active_enter_time = active_enter_time,
         main_pid = main_pid,
         memory_current = memory_current,
         restarts = int_strict(val("NRestarts"), "NRestarts")
    )
}
