% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare_inputs.R
\name{compare_non_retention_input_flags}
\alias{compare_non_retention_input_flags}
\title{Compares the non retention flags of two runs}
\usage{
compare_non_retention_input_flags(fram_db, run_ids, verbose = TRUE)
}
\arguments{
\item{fram_db}{FRAM database object}

\item{run_ids}{Two run ids}

\item{verbose}{If \code{TRUE}, print an update to screen when there are no differences in recruits.}
}
\description{
Compares the non retention flags of two runs
}
\examples{
\dontrun{fram_db |> compare_non_retention_inputs(c(55, 56))}
}
