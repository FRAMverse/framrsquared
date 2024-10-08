% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mortality_scaler.R
\name{mortality_scalers}
\alias{mortality_scalers}
\title{Guestimate the impact on a particular stock by multiplying its mortalities
by the \code{stock_mortality_ratio} produced by these functions.}
\usage{
mortality_scalers(fram_db, run_id, stock_id)
}
\arguments{
\item{fram_db}{FRAM database object}

\item{run_id}{Run ID}

\item{stock_id}{A focal stock or stocks}
}
\description{
Guestimate the impact on a particular stock by multiplying its mortalities
by the \code{stock_mortality_ratio} produced by these functions.
}
\examples{
\dontrun{fram_db |> mortality_scalers(run_id = 101, stock_id = c(17:18))}
}
