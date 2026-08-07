# systemd_unit_info(): fixture tests (always run) + live smoke.

fixdir <- if (dir.exists("fixtures")) {
    "fixtures"
} else {
    system.file("tinytest", "fixtures", package = "rsystemd")
}
fx <- function(f) readLines(file.path(fixdir, f))

fake <- function(lines, status = 0L) {
    function(cmd, args) list(status = status, output = lines)
}

# --- Recorded live unit parses to typed values ---

old <- rsystemd:::set_runner(fake(fx("systemctl-show-ssh.txt")))
info <- systemd_unit_info("ssh.service")
rsystemd:::set_runner(old)

expect_equal(info$unit, "ssh.service")
expect_equal(info$load_state, "loaded")
expect_equal(info$active_state, "active")
expect_equal(info$unit_file_state, "enabled")
expect_inherits(info$active_enter_time, "POSIXct")
expect_equal(attr(info$active_enter_time, "tzone"), "UTC")
expect_false(is.na(info$active_enter_time))
expect_true(is.integer(info$main_pid) && info$main_pid > 0L)
expect_true(is.numeric(info$memory_current) && info$memory_current > 0)
expect_equal(info$restarts, 0L)

# --- Recorded not-found unit RETURNS its record (absence is data):
# --- also covers MainPID=0, MemoryCurrent=[not set], empty timestamp ---

rsystemd:::set_runner(fake(fx("systemctl-show-notfound.txt")))
nf <- systemd_unit_info("nosuchunit-xyzzy.service")
rsystemd:::set_runner(old)

expect_equal(nf$load_state, "not-found")
expect_true(is.na(nf$main_pid))
expect_true(is.na(nf$memory_current))
expect_true(is.na(nf$active_enter_time))
expect_equal(nf$fragment_path, "")

# --- Fail-closed on non-key=value output and bad timestamps ---

rsystemd:::set_runner(fake(c("this is not key value")))
e <- tryCatch(systemd_unit_info("x.service"), error = identity)
rsystemd:::set_runner(old)
expect_inherits(e, "runix_parse_error")

rsystemd:::set_runner(fake(c("Id=x.service", "LoadState=loaded",
    "ActiveEnterTimestamp=not a date at all")))
e <- tryCatch(systemd_unit_info("x.service"), error = identity)
rsystemd:::set_runner(old)
expect_inherits(e, "runix_parse_error")

rsystemd:::set_runner(fake(c("Description=only", "SubState=dead")))
e <- tryCatch(systemd_unit_info("x.service"), error = identity)
rsystemd:::set_runner(old)
expect_inherits(e, "runix_parse_error")

# --- Input validation ---

expect_error(systemd_unit_info(c("a.service", "b.service")))
expect_error(systemd_unit_info(""))
expect_error(systemd_unit_info(NA_character_))

# --- Live smoke tests ---

if (at_home()) {
    live <- systemd_unit_info("systemd-journald.service")
    expect_equal(live$unit, "systemd-journald.service")
    expect_equal(live$active_state, "active")
    expect_true(live$main_pid > 0L)
    gone <- systemd_unit_info("nosuchunit-xyzzy.service")
    expect_equal(gone$load_state, "not-found")
}
