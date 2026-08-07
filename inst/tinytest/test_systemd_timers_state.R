# systemd_timers() and systemd_state(): fixture tests + live smoke.

fixdir <- if (dir.exists("fixtures")) {
    "fixtures"
} else {
    system.file("tinytest", "fixtures", package = "rsystemd")
}
fx <- function(f) readLines(file.path(fixdir, f), warn = FALSE)

# Both functions aggregate over systemd_units(); dispatch on args[1].
fake_agg <- function(primary, units_json, primary_status = 0L) {
    function(cmd, args) {
        if (args[1L] == "list-units") {
            list(status = 0L, output = units_json)
        } else {
            list(status = primary_status, output = primary)
        }
    }
}

tcols <- c("timer", "next_elapse", "last_trigger", "activates",
    "active_state")
units_json <- paste0('[{"unit":"sysstat-collect.timer","load":"loaded",',
    '"active":"active","sub":"waiting","description":"x"},',
    '{"unit":"broken.service","load":"loaded","active":"failed",',
    '"sub":"failed","description":"y"}]')

# --- Recorded timers JSON parses and joins active_state ---

old <- rsystemd:::set_runner(fake_agg(fx("systemctl-list-timers.json"),
    units_json))
tm <- systemd_timers()
rsystemd:::set_runner(old)

expect_equal(names(tm), tcols)
expect_true("sysstat-collect.timer" %in% tm$timer)
expect_inherits(tm$next_elapse, "POSIXct")
expect_equal(attr(tm$next_elapse, "tzone"), "UTC")
expect_true(all(nzchar(tm$activates)))
expect_equal(tm$active_state[tm$timer == "sysstat-collect.timer"], "active")

# --- null next/last become NA ---

rsystemd:::set_runner(fake_agg(paste0('[{"next":null,"left":null,',
    '"last":null,"passed":null,"unit":"idle.timer",',
    '"activates":"idle.service"}]'), units_json))
idle <- systemd_timers()
rsystemd:::set_runner(old)
expect_true(is.na(idle$next_elapse))
expect_true(is.na(idle$last_trigger))

# --- Empty and malformed timer output ---

rsystemd:::set_runner(fake_agg("[]", units_json))
t0 <- systemd_timers()
rsystemd:::set_runner(old)
expect_equal(nrow(t0), 0L)
expect_equal(names(t0), tcols)

rsystemd:::set_runner(fake_agg('[{"unit":"x.timer"}]', units_json))
e <- tryCatch(systemd_timers(), error = identity)
rsystemd:::set_runner(old)
expect_inherits(e, "runix_parse_error")

# --- systemd_state(): non-zero exit is data, failed units aggregate ---

rsystemd:::set_runner(fake_agg("degraded", units_json,
    primary_status = 1L))
st <- systemd_state()
rsystemd:::set_runner(old)
expect_equal(st$state, "degraded")
expect_equal(st$failed_units, "broken.service")

rsystemd:::set_runner(fake_agg("running", units_json))
st2 <- systemd_state()
rsystemd:::set_runner(old)
expect_equal(st2$state, "running")

rsystemd:::set_runner(fake_agg(character(), units_json))
e <- tryCatch(systemd_state(), error = identity)
rsystemd:::set_runner(old)
expect_inherits(e, "runix_parse_error")

# --- Live smoke tests ---

if (at_home()) {
    lt <- systemd_timers()
    expect_true(nrow(lt) > 3L)
    expect_true("anacron.timer" %in% lt$timer)
    expect_true(all(is.na(lt$active_state) |
        nzchar(lt$active_state)))

    ls <- systemd_state()
    expect_true(ls$state %in% c("running", "degraded"))
    expect_true(is.character(ls$failed_units))
    if (ls$state == "running") {
        expect_equal(length(ls$failed_units), 0L)
    }
}
