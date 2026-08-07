% tinyrox says don't edit this manually, but it can't stop you!
\name{systemd_journal}
\alias{systemd_journal}
\title{Read systemd journal entries}
\usage{
systemd_journal(
  unit = NULL,
  priority = NULL,
  since = NULL,
  until = NULL,
  n = 1000L
)
}
\arguments{
\item{unit}{Optional single unit name or glob to filter on.}

\item{priority}{Optional single integer 0-7: keep entries at that
severity or more severe (journalctl semantics).}

\item{n}{Maximum number of entries to return (most recent), default 1000.}

\item{since,until}{Optional single timestamp strings, passed to
journalctl verbatim (it accepts e.g. "2026-08-07 12:00:00",
"yesterday", "-2h").}
}
\value{
A data.frame with columns \code{time} (POSIXct, UTC),
  \code{priority} (integer 0-7), \code{unit}, \code{pid} (integer), and
  \code{message}. Fields a journal entry does not carry (kernel messages
  have no unit, for example) are \code{NA}. What is visible depends on
  the caller's journal permissions (adm/systemd-journal group membership
  for the full journal).
}
\description{
Queries the journal via journalctl's JSON machine output and returns the
newest entries, oldest first (newest last).
}
\examples{
\dontrun{
logs <- systemd_journal(priority = 3L, n = 100L)
logs[, c("time", "unit", "message")]
}
}
