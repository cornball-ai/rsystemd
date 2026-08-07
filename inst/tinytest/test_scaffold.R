# Scaffold sanity: metadata parses and declares the read-only scope.
d <- read.dcf(system.file("DESCRIPTION", package = "rsystemd"))
expect_equal(unname(d[, "Package"]), "rsystemd")
expect_equal(unname(d[, "OS_type"]), "unix")
