% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bits.R
\name{find_with_timestamp}
\alias{find_with_timestamp}
\alias{update_one_with_timestamp}
\alias{update_many_with_timestamp}
\title{Find and update records that have a timestamp field.}
\usage{
find_with_timestamp(
  collection,
  fields = "{}",
  ...,
  ts_field = "modified",
  id_field = "_id"
)

update_one_with_timestamp(
  document,
  collection,
  ts_field = "modified",
  id_field = "_id"
)

update_many_with_timestamp(
  document,
  collection,
  ...,
  simplify = TRUE,
  verbose = TRUE
)
}
\arguments{
\item{collection}{A mongo collection object created with
\code{\link{mongo}()}}

\item{fields}{Fields to be retrieved; see \code{\link{mongo}$find()}.}

\item{...}{Other parameters to be passed to mongo$find()}

\item{ts_field}{Name of field containing the timestamp.}

\item{id_field}{Name of field containing the mongo record id}

\item{document}{The list (or one-record data.frame) to be written to the
mongodb collection.}

\item{simplify}{passed to \code{\link{sapply}()}}
}
\value{
\code{find_with_timestamp()} returns a data.frame with the id and ts
  fields.

  \code{update_one_with_timestamp()} returns a list with modifiedCount,
  matchedCount, upsertedCount, message ("OK" or an error message) and the
  current value of the document that matches id from database.  If the update
  was successful, the returned record should have the values in
  \code{document}.  If the update was not successful (typically, the time
  stamp didn't match), the returned record will be the values in the record
  matching id_field in the database.

  \code{update_many_with_timestamp()} returns a matrix where each column
  contains the return value from the call to \code{update_one_with_timestamp}
}
\description{
Functions to prevent collisions with multi-user mongo databases.
}
\details{
\code{find_with_timestamp()} required that the queried table have a timestamp
field and that the mongo id field be returned in the query.  "fields"
parameter, if passed, must request the id_field and ts_field.

\code{update_one_with_timestamp} takes in the \code{document} parameters a
list (or a one-record data.frame) with members (or columns) named for
id_field and ts_field.  It will update the record that matches the id_field
value using the other values in \code{document} only if the timestamp in
\code{document} matches the time stamp in the database. If the update is
successful, the timestamp in the database is updated to the time of the
update.

\code{update_many_with_timestamp} is a simple convenience wrapper for
\code{update_one_with_timestamp}.  `document` can be a multi-record
data.frame. Use with caution.  An individual update attempt will be made for
every row in the data.frame regardless of errors in prior rows, meaning that
some records could succeed while others fail.  Ideally this would be done as
a transaction, but that's for later implementation.
}
