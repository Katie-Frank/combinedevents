#' Combined events results
#'
#' \code{combined_events()} is a generic function used to present results
#' of calls to \code{\link[=scores]{scores()}} and \code{\link[=marks]{marks()}}.
#'
#' @param marks a numeric vectors of marks
#' @param scores an integer vector of scores
#' @param event_names a character vector of event names
#' @param event a character string indicating the combined events competition
#' @param seconds a numeric (either 0 or 1)
#' @param ... other arguments passed on to methods
#'
#' @return An object of class "\code{combined_events}".
#' The default method returns a list of that class.
#' @seealso \code{\link[=scores]{scores()}}, \code{\link[=marks]{marks()}}
#' @export
#'
combined_events <- function(marks, scores, event_names, event, seconds, ...){
  UseMethod("combined_events")
}

#' @export
combined_events.default <- function(marks, scores, event_names, event, seconds, ...){
  total <- sum(scores, na.rm = TRUE)
  if (seconds == FALSE) {
    marks <- mapply(num_to_char, marks, event_names)
  }
  result <- list(results = data.frame(event = c(event_names, "TOTAL"),
                                      mark = c(unlist(marks), NA),
                                      score = c(scores, total)),
                 marks = stats::setNames(unlist(marks), event_names),
                 scores = stats::setNames(scores, event_names),
                 score_total = total)
  names(result$results)[1] <- event
  class(result) <- "combined_events"
  result
}

#' @export
print.combined_events <- function(x, ...){
  print(x$results)
}
