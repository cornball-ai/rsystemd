# systemd_units(): fixture tests (always run) + live smoke (at_home only).

fixdir <- if (dir.exists("fixtures")) {
    "fixtures"
} else {
    system.file("tinytest", "fixtures", package = "rsystemd")
}
fx <- function(f) readLines(file.path(fixdir, f), warn = FALSE)

fake <- function(lines, status = 0L) {
    function(cmd, args) list(status = status, output = lines)
}

cols <- c("unit", "load_state", "active_state", "sub_state", "description")

# --- Recorded real JSON parses to the contracted shape ---

old <- rsystemd:::set_runner(fake(fx("systemctl-list-units.json")))
df <- systemd_units()
rsystemd:::set_runner(old)

expect_inherits(df, "data.frame")
expect_equal(names(df), cols)
expect_true(nrow(df) > 3L)
expect_true("systemd-journald.service" %in% df$unit)
expect_true(all(nzchar(df$unit)))

# --- Fail-closed on truncated JSON ---

rsystemd:::set_runner(fake(fx("systemctl-list-units-truncated.json")))
e <- tryCatch(systemd_units(), error = identity)
rsystemd:::set_runner(old)
expect_inherits(e, "runix_parse_error")
expect_inherits(e, "rsystemd_error")
expect_inherits(e, "runix_error")

# --- Fail-closed on missing fields ---

rsystemd:::set_runner(fake(fx("systemctl-list-units-missing-fields.json")))
e <- tryCatch(systemd_units(), error = identity)
rsystemd:::set_runner(old)
expect_inherits(e, "runix_parse_error")

# --- Empty JSON array means zero rows, contracted columns ---

rsystemd:::set_runner(fake("[]"))
df0 <- systemd_units()
rsystemd:::set_runner(old)
expect_equal(nrow(df0), 0L)
expect_equal(names(df0), cols)

# --- Non-zero exit status is an error ---

rsystemd:::set_runner(fake(character(), status = 1L))
e <- tryCatch(systemd_units(), error = identity)
rsystemd:::set_runner(old)
expect_inherits(e, "rsystemd_error")

# --- Bad pattern argument refused before any command runs ---

expect_error(systemd_units(pattern = c("a", "b")))
expect_error(systemd_units(pattern = NA_character_))

# --- Missing backend tool is a typed error ---

e <- tryCatch(rsystemd:::run_system("no-such-tool-xyzzy", character()),
    error = identity)
expect_inherits(e, "rsystemd_missing_tool")

# --- Live smoke tests ---

if (at_home()) {
    live <- systemd_units()
    expect_equal(names(live), cols)
    expect_true(nrow(live) > 50L)
    expect_true("-.mount" %in% live$unit)
    expect_true("dbus.service" %in% live$unit)
    filtered <- systemd_units(pattern = "systemd-journal*")
    expect_true(nrow(filtered) >= 1L)
    expect_true(all(grepl("^systemd-journal", filtered$unit)))
    none <- systemd_units(pattern = "no-such-unit-xyzzy*")
    expect_equal(nrow(none), 0L)
    expect_equal(names(none), cols)
}
