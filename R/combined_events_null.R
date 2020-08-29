#' Combined events null results
#'
#' \code{combined_events_null()} is a generic function used to present results
#' of calls to \code{\link[=scores]{scores()}} and \code{\link[=marks]{marks()}}
#' where in those calls \code{combined_event = NULL}.
#'
#' @param marks a numeric vector of marks
#' @param scores an integer vector of scores
#' @param event_names a character vector of event names
#' @param seconds a numeric (either 0 or 1)
#' @param ... other arguments passed on to methods
#'
#' @return An object of class \code{combined_events_null}.
#' The default method returns a list of that class.
#' @seealso \code{\link[=scores]{scores()}}, \code{\link[=marks]{marks()}}
#' @export
combined_events_null <- function(marks, scores, event_names, seconds, ...){
  UseMethod("combined_events_null")
}

#' Default method for combined events null results
#'
#' @param marks a numeric vector of marks
#' @param scores an integer vector of scores
#' @param event_names a character vector of event names
#' @param seconds a numeric (either 0 or 1)
#' @param ... other arguments passed on to methods
#'
#' @details The default method of the generic function
#' \code{combined_events_null()} presents the results of calls to
#' \code{\link[=scores]{scores()}} and \code{\link[=marks]{marks()}} in a list.
#' @rdname combined_events_null
#' @method combined_events_null default
#' @export
combined_events_null.default <- function(marks, scores, event_names, seconds, ...){
  if (seconds == FALSE) {
    marks <- mapply(num_to_char, marks, event_names)
  }
  result <- list(results = data.frame(event = event_names,
                                      mark = unlist(marks),
                                      score = scores),
                 marks = stats::setNames(unlist(marks), event_names),
                 scores = stats::setNames(scores, event_names))
  class(result) <- "combined_events_null"
  result
}

#' @export
print.combined_events_null <- function(x, ...){
  print(x$results)
}
