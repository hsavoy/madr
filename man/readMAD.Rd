% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readMAD.R
\name{readMAD}
\alias{readMAD}
\title{Read the SQLite databases from MAD# into the MADproject object.}
\usage{
readMAD(proj, location)
}
\arguments{
\item{proj}{The MADproject object with the slots \code{madname},
\code{resultname}, and \code{xpath} specified.}
}
\value{
proj An updated MADproject object with slots \code{numTimesteps},
   \code{numLocations}, \code{numSamples}, \code{numAnchors},
   \code{numTheta}, \code{observations}, \code{priors},
   \code{truevalues} (if present),
   and \code{realizations} filled in from the MAD# databases.
}
\description{
\code{readMAD} returns an updated MADproject object with data from
the MAD# databases.
}

