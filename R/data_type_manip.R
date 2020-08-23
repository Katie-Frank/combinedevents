char_to_num <- function(x){
  if (is.na(x)) {
    return(as.numeric(x))
  } else if (!(stringr::str_detect(x, ":"))) {
    x <- as.numeric(x)
  } else {
    x <- lubridate::period_to_seconds(lubridate::ms(x))
  }
  x
}

num_to_char <- function(x, names){
  if (names %in% c("1500m", "1000m", "800m") & !is.na(x)) {
    x <- lubridate::seconds_to_period(x)
    paste0(x@minute, ":", stringr::str_pad(format(x@.Data, nsmall = 2), 5, pad = "0"))
  } else {
    x
  }
}
