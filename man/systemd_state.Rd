% tinyrox says don't edit this manually, but it can't stop you!
\name{systemd_state}
\alias{systemd_state}
\title{Overall system manager state}
\usage{
systemd_state()
}
\value{
A list with \code{state} (e.g. \code{"running"},
  \code{"degraded"}) and \code{failed_units} (character vector, empty
  when none).
}
\description{
The system manager's own verdict plus the failed units behind it.
Aggregation: the state word comes from \code{systemctl
is-system-running}; \code{failed_units} collects
\code{\link{systemd_units}} rows with \code{active_state "failed"}.
}
\examples{
\dontrun{
systemd_state()
}
}
