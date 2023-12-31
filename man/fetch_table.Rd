% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch_data.R
\name{fetch_table}
\alias{fetch_table}
\title{Fetches a complete table from a FRAM database. Returns a cleaned
tibble.}
\usage{
fetch_table(fram_db, table_name)
}
\arguments{
\item{fram_db}{FRAM database object}

\item{table_name}{Table to be fetched}
}
\description{
Fetches a complete table from a FRAM database. Returns a cleaned
tibble.
}
\examples{
\dontrun{fram_db |> fetch_table('Mortality')}

}
