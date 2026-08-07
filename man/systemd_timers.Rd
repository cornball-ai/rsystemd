% tinyrox says don't edit this manually, but it can't stop you!
\name{systemd_timers}
\alias{systemd_timers}
\title{List systemd timers}
\usage{
systemd_timers()
}
\value{
A data.frame with columns \code{timer}, \code{next_elapse}
  (POSIXct UTC, NA when not scheduled), \code{last_trigger} (POSIXct
  UTC, NA when never triggered), \code{activates}, and
  \code{active_state}.
}
\description{
All timers (including inactive), with their next and last trigger times
and the unit each activates. Aggregation: the trigger times come from
systemctl's list-timers JSON (usec epoch), the timer's own
\code{active_state} is joined from \code{\link{systemd_units}}.
}
\examples{
\dontrun{
systemd_timers()
}
}
