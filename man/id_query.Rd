% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bits.R
\name{id_query}
\alias{id_query}
\alias{id_time_query}
\title{Make a query that will match the oid (character) and date (POSIXt) contained
in a dataframe.}
\usage{
id_query(x, id_field = "_id", as_list = F)

id_time_query(x, ts_field = "modified", id_field = "_id")
}
\arguments{
\item{x}{a dataframe containing a character column representing an oid to
match and a POSIXt column representing a date to match.}

\item{id_field}{The name of the field containing the oid}

\item{ts_field}{The name of the field containing the date}
}
\value{
A JSON query string or a list that can be passed to jsonlite::toJSON.
}
\description{
Make a query that will match the oid (character) and date (POSIXt) contained
in a dataframe.
}
