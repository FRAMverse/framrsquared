% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/report_population_statistics.R
\name{population_statistics}
\alias{population_statistics}
\title{Returns a tibble matching the Population Statistics screen.}
\usage{
population_statistics(fram_db, run_id = NULL)
}
\arguments{
\item{fram_db}{FRAM database object}

\item{run_id}{Run ID}
}
\description{
Returns a tibble matching the Population Statistics screen.
}
\examples{
\dontrun{fram_db |> population_statistics(run_id = 101)}

}
