% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare_inputs.R
\name{compare_recruits}
\alias{compare_recruits}
\title{Compares the recruit scalers of two runs}
\usage{
compare_recruits(fram_db, run_ids, tolerance = 0.01, verbose = TRUE)
}
\arguments{
\item{fram_db}{FRAM database object}

\item{run_ids}{Two run ids}

\item{tolerance}{Tolerance for detecting changes}

\item{verbose}{If \code{TRUE}, print an update to screen when there are no differences in recruits.}
}
\description{
Compares the recruit scalers of two runs
}
\examples{
\dontrun{fram_db |> compare_recruits()}
}
