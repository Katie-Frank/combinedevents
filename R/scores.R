#' Calculate scores for track and field combined events
#'
#' \code{scores()} calculates scores for track and field combined events competitions.
#' This function is based on the IAAF Scoring Tables for Combined Events.
#'
#' @param marks a numeric or character vector of track and field
#'   marks/performances
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
#'
#' @return A list of class "\code{combined_events}" (or "\code{combined_events_null}" if \code{combined_event = NULL}) with
#'   the following fields:
#'   \item{results}{if called with non-NULL \code{combined_event}, a data frame with
#'   columns for the specified combined event containing the names of those events, \code{mark}
#'   for the input marks/performances, and \code{score} for the resulting scores based on those
#'   marks. The last row of the data frame gives the total score for the specified combined
#'   events competition. If \code{combined_event = NULL}, a data frame with columns \code{event},
#'   \code{mark}, and \code{score}.}
#'   \item{marks}{the vector of marks for the specified combined event. If not all marks were
#'   supplied to \code{scores()}, then there will be \code{NA} values for those events with
#'   missing marks. If \code{combined_event = NULL}, the vector of marks.}
#'   \item{scores}{the vector of scores based on the input marks for the specified combined event.
#'   If not all marks were supplied to \code{scores()}, then there will be scores with \code{NA}
#'   values for those events with missing marks. If \code{combined_event = NULL}, the vector of scores.}
#'   \item{score_total}{if called with non-NULL \code{combined_event}, an integer representing the overall score
#'   for the specified combined events competition}
#'   \item{call}{the matched call}
#' @export
#' @importFrom magrittr %>%
#' @examples
#' # Men's decathlon
#' scores(marks = c("100m" = 11.61, LJ = 7.32, SP = 13.17, HJ = 1.9,
#'                  "400m" = 49.96, "110mH" = 15.32, DT = 38.18, PV = 4.6,
#'                  JT = 58.98, "1500m" = "4:39.34"),
#'        gender = "male", combined_event = "decathlon")
#'
#' # Women's heptathlon
#' scores(c(14.11, 1.95, 13.96, 25.61, 6.44, 45.98, "2:07.26"),
#'        "female", "heptathlon")
#'
#' # Men's events
#' scores(c("60m" = 7.09, LJ = 7, LJ = 7.03, SP = 11.8, HJ = 2,
#'          "60mH" = 8.30, "60mH" = 9.31, PV = 4.30, "1000m" = "2:40.00"),
#'        gender = "male")
scores <- function(marks, gender, combined_event = NULL, seconds = FALSE){
  if (!class(marks) %in% c("numeric", "character")) {
    stop("`marks` must be a numeric or character vector")
  }
  if (any(marks < 0 & !is.na(marks))) {
    stop("Invalid entry for `marks`: negative mark(s) not allowed")
  }
  if (is.character(marks)) {
    marks <- as.list(marks) %>% sapply(char_to_num)
  }
  if(!isTRUE(all.equal(round(marks, 2), marks))) {
    message("One or more entries of `marks` have been rounded to the second decimal place")
    marks <- round(marks, 2)
  }
  if (gender == "male") {
    if (is.null(combined_event)) {
      if (is.null(names(marks))) {
        stop("Every element of `marks` must be named if `combined_event` is unspecified")
      } else if (!all(names(marks) %in% c("100m", "LJ", "SP", "HJ", "400m",
                                          "110mH", "DT", "PV", "JT", "1500m", "200m", "60m", "60mH", "1000m"))) {
        stop("One or more invalid names for `marks`")
      } else {
        scores <- mapply(exec_fun, fn = paste0(names(marks), "_men"), x = marks) %>%
          combined_events_null(marks, names(marks), seconds)
      }
    } else if (!(combined_event %in% c("decathlon", "outdoor decathlon",
                                       "outdoor pentathlon", "heptathlon", "indoor heptathlon",
                                       "indoor pentathlon"))) {
      stop("Invalid entry for `combined_event`")
    } else if (combined_event == "decathlon" | combined_event == "outdoor decathlon") {
      scores <- do.call(dec_men, as.list(c(seconds, marks)))
    } else if (combined_event == "outdoor pentathlon") {
      scores <- do.call(penta_men_out, as.list(c(seconds, marks)))
    } else if (combined_event == "heptathlon" | combined_event == "indoor heptathlon") {
      scores <- do.call(hept_men, as.list(c(seconds, marks)))
    } else if (combined_event == "indoor pentathlon") {
      scores <- do.call(penta_men_in, as.list(c(seconds, marks)))
    }
  } else if (gender == "female") {
    if (is.null(combined_event)) {
      if (is.null(names(marks))) {
        stop("Every element of `marks` must be named if `combined_event` is unspecified")
      } else if (!all(names(marks) %in% c("100m", "LJ", "SP", "HJ", "400m",
                                          "100mH", "DT", "PV", "JT", "1500m", "200m", "60mH", "800m"))) {
        stop("One or more invalid names for `marks`")
      } else {
        scores <- mapply(exec_fun, fn = paste0(names(marks), "_women"), x = marks) %>%
          combined_events_null(marks, names(marks), seconds)
      }
    } else if (!(combined_event %in% c("heptathlon", "outdoor heptathlon", "decathlon",
                                       "outdoor decathlon", "pentathlon",
                                       "indoor heptathlon"))) {
      stop("Invalid entry for `combined_event`")
    } else if (combined_event == "heptathlon" | combined_event == "outdoor heptathlon") {
      scores <- do.call(hept_women, as.list(c(seconds, marks)))
    } else if (combined_event == "decathlon" | combined_event == "outdoor decathlon") {
      scores <- do.call(dec_women, as.list(c(seconds, marks)))
    } else if (combined_event == "pentathlon" | combined_event == "indoor pentathlon") {
      scores <- do.call(penta_women, as.list(c(seconds, marks)))
    }
  }
  scores$call <- match.call()
  scores
}

exec_fun <- function(fn, x) {
  rlang::exec(fn, x)
}
