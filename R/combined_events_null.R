#' @export
combined_events_null <- function(marks, scores, event_names, seconds, ...){
  UseMethod("combined_events_null")
}

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
