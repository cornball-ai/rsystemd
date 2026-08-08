% tinyrox says don't edit this manually, but it can't stop you!
\name{systemd_restart}
\alias{systemd_restart}
\title{Restart a systemd unit}
\usage{
systemd_restart(unit, scope = "system", dry_run = FALSE, timeout = 90)
}
\arguments{
\item{unit}{A single unit name.}

\item{scope}{\code{"system"} (default) or \code{"user"}.}

\item{dry_run}{Preview only; issue no effect.}

\item{timeout}{Seconds to wait for the job to settle (default 90).}
}
\value{
A \code{systemd_result}.
}
\description{
Always issues the effect (a restart is never idempotent) and confirms a
fresh invocation ran before reporting success.
}
