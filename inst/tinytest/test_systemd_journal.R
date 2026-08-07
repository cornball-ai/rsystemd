# systemd_journal(): fixture tests (always run) + live smoke (at_home only).

fixdir <- if (dir.exists("fixtures")) {
    "fixtures"
} else {
    system.file("tinytest", "fixtures", package = "rsystemd")
}
fx <- function(f) readLines(file.path(fixdir, f), warn = FALSE)

fake <- function(lines, status = 0L, seen = NULL) {
    function(cmd, args) {
        if (!is.null(seen)) {
            seen$args <- args
        }
        list(status = status, output = lines)
    }
}

cols <- c("time", "priority", "unit", "pid", "message")

# --- Recorded real NDJSON parses to the contracted shape ---

old <- rsystemd:::set_runner(fake(fx("journalctl-json.txt")))
df <- systemd_journal(n = 20L)
rsystemd:::set_runner(old)

expect_inherits(df, "data.frame")
expect_equal(names(df), cols)
expect_equal(nrow(df), 20L)
expect_inherits(df$time, "POSIXct")
expect_equal(attr(df$time, "tzone"), "UTC")
expect_true(all(df$priority %in% c(0:7, NA)))
expect_true(all(is.na(df$pid) | df$pid > 0L))
expect_true(any(!is.na(df$message)))

# --- Argument construction ---

seen <- new.env()
rsystemd:::set_runner(fake(character(), seen = seen))
systemd_journal(unit = "ssh.service", priority = 3L, since = "-2h",
    n = 50L)
rsystemd:::set_runner(old)
expect_true("--lines=50" %in% seen$args)
expect_true("--priority=3" %in% seen$args)
expect_true("--unit" %in% seen$args)
expect_true(any(grepl("^--since=", seen$args)))

# --- Empty output means zero rows with contracted columns ---

rsystemd:::set_runner(fake(character()))
df0 <- systemd_journal()
rsystemd:::set_runner(old)
expect_equal(nrow(df0), 0L)
expect_equal(names(df0), cols)

# --- Fail-closed on truncated JSON ---

rsystemd:::set_runner(fake(fx("journalctl-json-truncated.txt")))
e <- tryCatch(systemd_journal(), error = identity)
rsystemd:::set_runner(old)
expect_inherits(e, "runix_parse_error")
expect_inherits(e, "rsystemd_error")

# --- Fail-closed on missing timestamp ---

rsystemd:::set_runner(fake('{"MESSAGE":"no timestamp here"}'))
e <- tryCatch(systemd_journal(), error = identity)
rsystemd:::set_runner(old)
expect_inherits(e, "runix_parse_error")

# --- Byte-array fields (non-UTF-8) convert to character ---

rsystemd:::set_runner(fake(paste0('{"__REALTIME_TIMESTAMP":"1754600000000000",',
    '"PRIORITY":"6","MESSAGE":[104,105]}')))
db <- systemd_journal()
rsystemd:::set_runner(old)
expect_equal(db$message, "hi")

# --- Non-zero exit status is an error ---

rsystemd:::set_runner(fake(character(), status = 1L))
e <- tryCatch(systemd_journal(), error = identity)
rsystemd:::set_runner(old)
expect_inherits(e, "rsystemd_error")

# --- Input validation ---

expect_error(systemd_journal(n = 0L))
expect_error(systemd_journal(priority = 9L))
expect_error(systemd_journal(priority = c(1L, 2L)))
expect_error(systemd_journal(unit = c("a.service", "b.service")))
expect_error(systemd_journal(since = ""))

# --- Live smoke tests ---

if (at_home()) {
    live <- systemd_journal(n = 25L)
    expect_equal(names(live), cols)
    expect_true(nrow(live) > 0L)
    expect_true(all(diff(as.numeric(live$time)) >= -1))

    errs <- systemd_journal(priority = 3L, n = 10L)
    expect_true(all(stats::na.omit(errs$priority) <= 3L))

    jd <- systemd_journal(unit = "systemd-journald.service", n = 10L)
    expect_true(all(is.na(jd$unit) | jd$unit == "systemd-journald.service"))
}
