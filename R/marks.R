#' Calculate marks for track and field combined events
#'
#' \code{marks()} calculates marks for track and field combined events competitions.
#' This function evolved from the IAAF Scoring Tables for Combined Events.
#'
#' @param scores a numeric vector of track and field scores
#' @param gender gender of athlete; either "\code{male}" or "\code{female}"
#' @param combined_event an optional character string indicating the
#'   combined events competition. For \code{gender = "male"}, the options are
#'   "\code{decathlon}"/"\code{outdoor decathlon}", "\code{outdoor pentathlon}",
#'   "\code{heptathlon}"/"\code{indoor heptathlon}", and "\code{indoor pentathlon}".
#'   For \code{gender = "female"}, the options are "\code{heptathlon}"/"\code{outdoor heptathlon}",
#'   "\code{decathlon}"/"\code{outdoor decathlon}", and "\code{pentathlon}"/"\code{indoor pentathlon}".
#'   If \code{combined_event = NULL}, the elements of \code{marks} must be named.
#'   \itemize{
#'     \item For \code{gender = "male"}, the allowed names for the performances
#'     in \code{marks} are \code{`100m`}, \code{LJ}, \code{SP}, \code{HJ}, \code{`400m`}, \code{`110mH`}, \code{DT}, \code{PV}, \code{JT}, \code{`1500m`},
#'     \code{`200m`}, \code{`60m`}, \code{`60mH`}, and \code{`1000m`}.
#'     \item For \code{gender = "female"}, the allowed names are \code{`100m`}, \code{LJ},
#'     \code{SP}, \code{HJ}, \code{`400m`}, \code{`100mH`}, \code{DT}, \code{PV}, \code{JT}, \code{`1500m`}, \code{`200m`}, \code{`60mH`}, and \code{`800m`}.
#'   }
#' @param seconds a logical; if \code{TRUE}, will return all track event marks in seconds
#' @details \code{marks()} can be thought of as the inverse function of \code{\link[=scores]{scores()}}: you
#' give it the scores you want to obtain, and it gives you the marks you
#' need to achieve those scores.
#'
#' For track events, \code{marks()} returns the slowest time needed to achieve the
#' input score. Similarly, for jumping and throwing events, \code{marks()} returns the
#' shortest distance necessary to achieve the input score.
#'
#' For some events, when a score is given to \code{marks()}, the score returned is
#' not necessarily the same as the one input because some scores are not actually
#' possible (due to rounding). In other words, not every input score is associated
#' with a unique mark. When this is the case, \code{marks()} will return the highest
#' score corresponding to that mark.
#' @return A list of class "\code{combined_events}" (or "\code{combined_events_null}" if \code{combined_event = NULL}) with
#'   the following fields:
#'   \item{results}{if called with non-NULL \code{combined_event}, a data frame with
#'   columns for the specified combined event containing the names of those events, \code{mark}
#'   for the resulting marks based on the input scores, and \code{score} based on the input
#'   scores. The last row of the data frame gives the total score for the specified combined
#'   events competition. If \code{combined_event = NULL}, a data frame with columns \code{event},
#'   \code{mark}, and \code{score}.}
#'   \item{marks}{the vector of marks based on the input scores for the specified combined event.
#'   If not all scores were supplied to \code{marks()}, then there will be \code{NA} values
#'   for those events with missing scores. If \code{combined_event = NULL}, the vector of marks.}
#'   \item{scores}{the vector of scores for the specified combined event. If not all scores
#'   were supplied to \code{marks()}, then there will be \code{NA} values for those events with
#'   missing scores. If \code{combined_event = NULL}, the vector of scores.}
#'   \item{score_total}{if called with non-NULL \code{combined_event}, an integer representing the overall score
#'   for the specified combined events competition}
#'   \item{call}{the matched call}
#' @export
#' @examples
#' # Men's heptathlon
#' marks(scores = rep(800, 7),
#'       gender = "male", combined_event = "heptathlon")
#'
#' # Women's pentathlon
#' marks(scores = c("60mH" = 981, HJ = 875, SP = 799, LJ = 956, `800m` = 1000),
#'       "female", "pentathlon")
#'
#' # Men's events
#' marks(scores = c(LJ = 790, LJ = 810, HJ = 850, HJ = 900, PV = 900, PV = 915),
#'       "male")
marks <- function(scores, gender, combined_event = NULL, seconds = FALSE){
  if (!typeof(scores) %in% c("integer", "double")) {
    stop("`scores` must be a numeric vector")
  }
  if (any(scores < 0 & !is.na(scores))) {
    stop("Invalid entry for `scores`: negative score(s) not allowed")
  }
  if(!isTRUE(all.equal(round(scores), scores))) {
    message("One or more entries of `scores` have been rounded to the nearest integer")
    scores <- round(scores)
  }
  if (gender == "male") {
    if (is.null(combined_event)) {
      if (is.null(names(scores))) {
        stop("Every element of `scores` must be named if `combined_event` is unspecified")
      } else if (!all(names(scores) %in% c("100m", "LJ", "SP", "HJ", "400m",
                                           "110mH", "DT", "PV", "JT", "1500m", "200m", "60m", "60mH", "1000m"))) {
        stop("One or more invalid names for `scores`")
      } else {
        marks <- mapply(exec_fun, paste0(names(scores), "_men_mark"), scores)
        names(marks) <- names(scores)
        scores <- mapply(exec_fun, paste0(names(scores), "_men"), marks) %>%
          combined_events_null(marks = marks, event_names = names(marks), seconds = seconds)
      }
    } else if (!(combined_event %in% c("decathlon", "outdoor decathlon",
                                       "outdoor pentathlon", "heptathlon", "indoor heptathlon",
                                       "indoor pentathlon"))) {
      stop("Invalid entry for `combined_event`")
    } else if (combined_event == "decathlon" | combined_event == "outdoor decathlon") {
      scores <- do.call(dec_men_mark, as.list(c(seconds, scores)))
    } else if (combined_event == "outdoor pentathlon") {
      scores <- do.call(penta_men_out_mark, as.list(c(seconds, scores)))
    } else if (combined_event == "heptathlon" | combined_event == "indoor heptathlon") {
      scores <- do.call(hept_men_mark, as.list(c(seconds, scores)))
    } else if (combined_event == "indoor pentathlon") {
      scores <- do.call(penta_men_in_mark, as.list(c(seconds, scores)))
    }
  } else if (gender == "female") {
    if (is.null(combined_event)) {
      if (is.null(names(scores))) {
        stop("Every element of `scores` must be named if `combined_event` is unspecified")
      } else if (!all(names(scores) %in% c("100m", "LJ", "SP", "HJ", "400m",
                                           "100mH", "DT", "PV", "JT", "1500m", "200m", "60mH", "800m"))) {
        stop("One or more invalid names for `scores`")
      } else {
        marks <- mapply(exec_fun, paste0(names(scores), "_women_mark"), scores)
        names(marks) <- names(scores)
        scores <- mapply(exec_fun, paste0(names(scores), "_women"), marks) %>%
          combined_events_null(marks = marks, event_names = names(marks), seconds = seconds)
      }
    } else if (!(combined_event %in% c("heptathlon", "outdoor heptathlon", "decathlon",
                                       "outdoor decathlon", "pentathlon",
                                       "indoor heptathlon"))) {
      stop("Invalid entry for `combined_event`")
    } else if (combined_event == "heptathlon" | combined_event == "outdoor heptathlon") {
      scores <- do.call(hept_women_mark, as.list(c(seconds, scores)))
    } else if (combined_event == "decathlon" | combined_event == "outdoor decathlon") {
      scores <- do.call(dec_women_mark, as.list(c(seconds, scores)))
    } else if (combined_event == "pentathlon" | combined_event == "indoor pentathlon") {
      scores <- do.call(penta_women_mark, as.list(c(seconds, scores)))
    }
  }
  scores$call <- match.call()
  scores
}
