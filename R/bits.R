#' Find and update records that have a timestamp field.
#'
#' Functions to prevent collisions with multi-user mongo databases.
#'
#' \code{find_with_timestamp()} required that the queried table have a timestamp
#' field and that the mongo id field be returned in the query.  "fields"
#' parameter, if passed, must request the id_field and ts_field.
#'
#' \code{update_one_with_timestamp} takes in the \code{document} parameters a
#' list (or a one-record data.frame) with members (or columns) named for
#' id_field and ts_field.  It will update the record that matches the id_field
#' value using the other values in \code{document} only if the timestamp in
#' \code{document} matches the time stamp in the database. If the update is
#' successful, the timestamp in the database is updated to the time of the
#' update.
#'
#' \code{update_many_with_timestamp} is a simple convenience wrapper for
#' \code{update_one_with_timestamp}.  `document` can be a multi-record
#' data.frame. Use with caution.  An individual update attempt will be made for
#' every row in the data.frame regardless of errors in prior rows, meaning that
#' some records could succeed while others fail.  Ideally this would be done as
#' a transaction, but that's for later implementation.
#'
#' @param document The list (or one-record data.frame) to be written to the
#'   mongodb collection.
#' @param collection A mongo collection object created with
#'   \code{\link{mongo}()}
#' @param fields Fields to be retrieved; see \code{\link{mongo}$find()}.
#' @param ts_field Name of field containing the timestamp.
#' @param id_field Name of field containing the mongo record id
#' @param ... Other parameters to be passed to mongo$find()
#' @param simplify passed to \code{\link{sapply}()}
#' @returns \code{find_with_timestamp()} returns a data.frame with the id and ts
#'   fields.
#'
#'   \code{update_one_with_timestamp()} returns a list with modifiedCount,
#'   matchedCount, upsertedCount, message ("OK" or an error message) and the
#'   current value of the document that matches id from database.  If the update
#'   was successful, the returned record should have the values in
#'   \code{document}.  If the update was not successful (typically, the time
#'   stamp didn't match), the returned record will be the values in the record
#'   matching id_field in the database.
#'
#'   \code{update_many_with_timestamp()} returns a matrix where each column
#'   contains the return value from the call to \code{update_one_with_timestamp}
#' @export
find_with_timestamp <-
  function (collection, fields = '{}', ..., ts_field = "modified", id_field = "_id") {
    resp <- collection$find(fields = fields, ...)
    if(!all(c(id_field, ts_field) %in% names(resp)))
      stop("Fields `", id_field, "` and `", ts_field, "` must be in the query result.")
    resp
  }

#' @rdname find_with_timestamp
#' @export
update_one_with_timestamp <-
  function(document, collection,
           ts_field = "modified", id_field = "_id") {

  if(!is.list(document)) stop("`document` must be a data.frame or a list")
  if(is.data.frame(document)) {
    if(nrow(document) != 1) stop("`document` must have only one row.")
    # "unbox" any column that is a list...
#    document <-
#      as.list(document) |>
#      lapply(
#        \(x) {
#          if(inherits(x, "list")) return(unlist(x, recursive = FALSE))
#          x
#        })
  }
  if(!all(c(id_field, ts_field) %in% names(document)))
    stop("`document` must have fields `", id_field, "` and `", ts_field,"`")

  query <- id_time_query(document, ts_field = ts_field)

  update =
    jsonlite::toJSON(
      list(
        # set ts_field to server time
        `$currentDate` =  setNames(list(TRUE), ts_field),
        # get rid of _id and ts_field for other values
        `$set` =
          as.list(document)[!names(document) %in% c(id_field, ts_field)]),
      POSIXt = "mongo", auto_unbox = TRUE)

  result <- collection$update(query, update)

  if(result$matchedCount == 0)
    result$message <- "Update failed.  Another user may have updated the same record."
  else
    result$message <- "OK"

  fields <-
    structure(
      as.list(rep(1, length(document))),
      names = names(document)) |>
    jsonlite::toJSON(auto_unbox = TRUE)
  query <- id_query(document)
  result$document <- collection$find(query = query, fields = fields)
  result
}

#' @rdname find_with_timestamp
#' @export
update_many_with_timestamp <- function(document, collection, ..., simplify = TRUE, verbose = TRUE) {
  if(!is.data.frame(document)) stop("`document` must be a data.frame")
  sapply(1:nrow(document),
         \(i) {
           if(verbose) cat("\r", i)
           update_one_with_timestamp(
             document[i,],
             collection,
             ...)
           },
           simplify = simplify)
}

#' Insert a data.frame into mongo with a creation time stamp.
#'
#' A wrapper around \code{\link{mongolite::mongo}$insert}.  Mongoserver is
#' queried for current time and that time is add to the data.frame in two
#' columns named using the values "ts_field" (modified time/date) and "cr_field"
#' (created time/date).
#' @param data A data.frame to add to a mongo collection.
#' @param collection A mongo colleciton object where the data will be stored.
#' @param ts_field Name of key for the modified time stamp.
#' @param cr_field Name of key for the created time stamp.
#' @returns The result from \code{\link{mongolite::mongo}$insert}.
insert_with_timestamp <- function(
    data, collection,
    ts_field = "modified", cr_field = "created", ...) {
  if(!is.list(data)) stop("`data` must be a data.frame")
  data[[cr_field]] <-
    as.POSIXct(
      collection$run('{"isMaster": 1}')$localTime,
      format = "%Y-%m-%dT%H:%M:%OSZ",
      tz = "UTC")
  data[[ts_field]] <- data[[cr_field]]
  collection$insert(data, ...)
}


#' Make a query that will match the oid (character) and date (POSIXt) contained
#' in a dataframe.
#' @param x a dataframe containing a character column representing an oid to
#' match and a POSIXt column representing a date to match.
#' @param id_field The name of the field containing the oid
#' @param ts_field The name of the field containing the date
#' @returns A JSON query string or a list that can be passed to jsonlite::toJSON.
#' @export
id_query <- function(x, id_field = "_id", as_list = F) {
  stopifnot(is.list(x), !is.null(names(x)))
  stopifnot(id_field %in% names(x))
  oid <- x[[id_field]]
  stopifnot(is.character(oid), !anyNA(oid), length(oid) > 0L)
  if (length(oid) == 1L) {
    q <- list("_id" = list("$oid" = oid))
  } else {
    q <- list("_id" = list("$in" = lapply(oid, function(x) list("$oid" = x))))
  }
  if(as_list) return(q)
  jsonlite::toJSON(q, auto_unbox = T, POSIXt = "mongo")
}

#' @rdname id_query
#' @export
id_time_query <- function(x, ts_field = "modified", id_field = "_id") {
  q1 <- id_query(x, id_field, as_list = T)
  stopifnot(ts_field %in% names(x))
  dates <- x[[ts_field]]
  stopifnot(inherits(dates, "POSIXt"), length(x[[id_field]]) == length(dates))
  q2 <- list(list("$in" = dates)) |> setNames(ts_field)

  jsonlite::toJSON(c(q1, q2), auto_unbox = TRUE, POSIXt = "mongo")
}

#' Tag singleton dates in a list for unboxing during conversion to JSON.
#'
#' Uses \code{\link{rapply()}} to search all levels of a list or object
#' built atop a list for POSIXt vectors.  Any POSIXt vector with length
#' of 1 will be marked for unboxing.  This function is useful because
#' \code{auto_unbox} in \code{\link{toJSON}} does not auto_unbox
#' POSIXt values.
#' @param document A list (or list-derived) object intended to be converted to
#'   JSON.
#' @returns A copy of the list with singleton POSIXt values tagged for unboxing.
#' @export
unbox_dates <- function(document) {
  if(!is.list(document)) stop("`document` must be a list or data.frame")
  rapply(
    document,
    \(x) {
      if(length(x) == 1) return(jsonlite::unbox(x))
      x
    },
    classes = c("POSIXt"),
    how = "replace")
}

#' Easily make JSON from named parameters.
#'
#' A shortcut for making JSON.  \code{j(a=1, b=2)} will return the same as
#' \code{jsonlite::toJSON(list(a=1, b=2))}.  Nested lists are allowed, e.g.,
#' \code{j(a=1, b=list(c=2, d=3))}
#' @param ... any named objects to be put in the JSON string.
#' @param args A named list of addition arguments to be passed to
#'   \code{\link{toJSON}}
#' @returns A JSON string.
#' @export
j <- function(..., args = list(auto_unbox = TRUE, POSIXt = "mongo")) {
  do.call(
    jsonlite::toJSON,
    c(list(x = list(...)), args)
  )
}

