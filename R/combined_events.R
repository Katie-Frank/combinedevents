#' @export
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
