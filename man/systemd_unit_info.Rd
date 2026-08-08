% tinyrox says don't edit this manually, but it can't stop you!
\name{systemd_unit_info}
\alias{systemd_unit_info}
\title{Typed properties of one systemd unit}
\usage{
systemd_unit_info(unit, scope = "system")
}
\arguments{
\item{unit}{A single unit name.}

\item{scope}{\code{"system"} (default) or \code{"user"} — which systemd
manager to query.}
}
\value{
A named list: \code{unit}, \code{description}, \code{load_state},
  \code{active_state}, \code{sub_state}, \code{unit_file_state},
  \code{fragment_path}, \code{active_enter_time} (POSIXct UTC, NA when
  never active), \code{main_pid} (integer, NA when none),
  \code{memory_current} (numeric bytes, NA when not set),
  \code{restarts} (integer), \code{invocation_id} (character, the
  systemd per-(re)start id, NA when the unit is not running), and
  \code{state_change_monotonic} (numeric microseconds since boot,
  monotonic, NA when unset). The last two are the correlation markers
  Phase 2 mutations use to confirm a fresh job ran.
}
\description{
Queries systemctl show for a documented subset of unit properties.
Absence of a unit is data here: a not-found unit returns its record with
\code{load_state "not-found"}, it is not an error.
}
\examples{
\dontrun{
info <- systemd_unit_info("ssh.service")
info$active_state
}
}
