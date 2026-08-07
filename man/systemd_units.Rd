% tinyrox says don't edit this manually, but it can't stop you!
\name{systemd_units}
\alias{systemd_units}
\title{List systemd units}
\usage{
systemd_units(pattern = NULL)
}
\arguments{
\item{pattern}{Optional single glob pattern (e.g. \code{"ssh*"}) to filter
unit names server-side.}
}
\value{
A data.frame with columns \code{unit}, \code{load_state},
  \code{active_state}, \code{sub_state}, and \code{description}. Zero rows
  when the pattern matches nothing.
}
\description{
Queries the system manager for all loaded units (including inactive ones)
via systemctl's JSON machine output.
}
\examples{
\dontrun{
units <- systemd_units()
units[units$active_state == "failed", ]
}
}
