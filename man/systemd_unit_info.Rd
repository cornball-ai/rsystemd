% tinyrox says don't edit this manually, but it can't stop you!
\name{systemd_unit_info}
\alias{systemd_unit_info}
\title{Typed properties of one systemd unit}
\usage{
systemd_unit_info(unit)
}
\arguments{
\item{unit}{A single unit name.}
}
\value{
A named list: \code{unit}, \code{description}, \code{load_state},
  \code{active_state}, \code{sub_state}, \code{unit_file_state},
  \code{fragment_path}, \code{active_enter_time} (POSIXct UTC, NA when
  never active), \code{main_pid} (integer, NA when none),
  \code{memory_current} (numeric bytes, NA when not set),
  \code{restarts} (integer).
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
