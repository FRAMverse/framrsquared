% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/report_fishery_mortality.R
\name{fishery_mortality}
\alias{fishery_mortality}
\title{Returns a tibble matching the Fishery Mortality screen.}
\usage{
fishery_mortality(fram_db, run_id = NULL, msp = TRUE)
}
\arguments{
\item{fram_db}{FRAM database object}

\item{run_id}{Run ID}

\item{msp}{Model Stock Proportion, default TRUE}
}
\description{
Returns a tibble matching the Fishery Mortality screen.
}
\examples{
\dontrun{
fram_db |> fishery_mortality(run_id = 101)
}
}
