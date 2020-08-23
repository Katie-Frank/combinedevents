`100m_men_mark` <- function(s, a = 25.4347, b = 18, c = 1.81){
  if (is.null(s)) {
    return(NA)
  } else if (is.na(s)) {
    return(NA)
  }
  if (s > 4753) {
    message("100m score must be <= 4753 points to receive a valid (positive) time")
    return(0)
  } else {
    floor((b - (s / a)^(1 / c)) * 100) / 100
  }
}

LJ_men_mark <- function(s, a = 0.14354, b = 220, c = 1.4){
  if (is.null(s)) {
    return(NA)
  } else if (is.na(s)) {
    return(NA)
  }
  ceiling(b + (s / a)^(1 / c)) / 100
}

SP_men_mark <- function(s, a = 51.39, b = 1.5, c = 1.05){
  if (is.null(s)) {
    return(NA)
  } else if (is.na(s)) {
    return(NA)
  }
  ceiling((b + (s / a)^(1 / c)) * 100) / 100
}

HJ_men_mark <- function(s, a = 0.8465, b = 75, c = 1.42){
  if (is.null(s)) {
    return(NA)
  } else if(is.na(s)) {
    return(NA)
  }
  ceiling(b + (s / a)^(1 / c)) / 100
}

`400m_men_mark` <- function(s, a = 1.53775, b = 82, c = 1.81){
  if (is.null(s)) {
    return(NA)
  } else if (is.na(s)) {
    return(NA)
  }
  if (s > 4474) {
    message("400m score must be <= 4474 points to receive a valid (positive) time")
    return(0)
  } else {
    floor((b - (s / a)^(1 / c)) * 100) / 100
  }
}

`110mH_men_mark` <- function(s, a = 5.74352, b = 28.5 , c = 1.92){
  if (is.null(s)) {
    return(NA)
  } else if (is.na(s)) {
    return(NA)
  }
  if (s > 3566) {
    message("110mH score must be <= 3566 points to receive a valid (positive) time")
    return(0)
  } else {
    floor((b - (s / a)^(1 / c)) * 100) / 100
  }
}

DT_men_mark <- function(s, a = 12.91, b = 4, c = 1.1){
  if (is.null(s)) {
    return(NA)
  } else if (is.na(s)) {
    return(NA)
  }
  ceiling((b + (s / a)^(1 / c)) * 100) / 100
}

PV_men_mark <- function(s, a = 0.2797, b = 100, c = 1.35){
  if (is.null(s)) {
    return(NA)
  } else if(is.na(s)) {
    return(NA)
  }
  ceiling(b + (s / a)^(1 / c)) / 100
}

JT_men_mark <- function(s, a = 10.14, b = 7, c = 1.08){
  if (is.null(s)) {
    return(NA)
  } else if(is.na(s)) {
    return(NA)
  }
  ceiling((b + (s / a)^(1 / c)) * 100) / 100
}

`1500m_men_mark` <- function(s, a = 0.03768, b = 480, c = 1.85){
  if (is.null(s)) {
    return(NA)
  } else if(is.na(s)) {
    return(NA)
  }
  if (s > 3438) {
    message("1500m score must be <= 3438 points to receive a valid (positive) time")
    return(0)
  } else {
    floor((b - (s / a)^(1 / c)) * 100) / 100
  }
}

`200m_men_mark` <- function(s, a = 5.8425, b = 38, c = 1.81){
  if (is.null(s)) {
    return(NA)
  } else if(is.na(s)) {
    return(NA)
  }
  if (s > 4224) {
    message("200m score must be <= 4224 points receive a valid (positive) time")
    return(0)
  } else {
    floor((b - (s / a)^(1 / c)) * 100) / 100
  }
}

`60m_men_mark` <- function(s, a = 58.0150, b = 11.5, c = 1.81){
  if (is.null(s)) {
    return(NA)
  } else if(is.na(s)) {
    return(NA)
  }
  if (s > 4816) {
    message("60m score must be <= 4816 points to receive a valid (positive) time")
    return(0)
  } else {
    floor((b - (s / a)^(1 / c)) * 100) / 100
  }
}

`60mH_men_mark` <- function(s, a = 20.5173, b = 15.5, c = 1.92){
  if (is.null(s)) {
    return(NA)
  } else if(is.na(s)) {
    return(NA)
  }
  if (s > 3953) {
    message("60mH score must be <= 3953 points to receive a valid (positive) time")
    return(0)
  } else {
    floor((b - (s / a)^(1 / c)) * 100) / 100
  }
}

`1000m_men_mark` <- function(s, a = 0.08713, b = 305.5, c = 1.85){
  if (is.null(s)) {
    return(NA)
  } else if(is.na(s)) {
    return(NA)
  }
  if (s > 3446) {
    message("1000m score must be <= 3446 points to receive a valid (positive) time")
    return(0)
  } else {
    floor((b - (s / a)^(1 / c)) * 100) / 100
  }
}

`100mH_women_mark` <- function(s, a = 9.23076, b = 26.7, c = 1.835) {
  if (is.null(s)) {
    return(NA)
  } else if(is.na(s)) {
    return(NA)
  }
  if (s > 3824) {
    message("100mH score must be <= 3824 points to receive a valid (positive) time")
    return(0)
  } else {
    floor((b - (s / a)^(1 / c)) * 100) / 100
  }
}

HJ_women_mark <- function(s, a = 1.84523, b = 75, c = 1.348) {
  if (is.null(s)) {
    return(NA)
  } else if (is.na(s)) {
    return(NA)
  }
  ceiling(b + (s / a)^(1 / c)) / 100
}

SP_women_mark <- function(s, a = 56.0211, b = 1.5, c = 1.05) {
  if (is.null(s)) {
    return(NA)
  } else if (is.na(s)) {
    return(NA)
  }
  ceiling((b + (s / a)^(1 / c)) * 100) / 100
}

`200m_women_mark` <- function(s, a = 4.99087, b = 42.50, c = 1.81) {
  if (is.null(s)) {
    return(NA)
  } else if (is.na(s)) {
    return(NA)
  }
  if (s > 4419) {
    message("200m score must be <= 4419 points receive a valid (positive) time")
    return(0)
  } else {
    floor((b - (s / a)^(1 / c)) * 100) / 100
  }
}

LJ_women_mark <- function(s, a = 0.188807, b = 210, c = 1.41){
  if (is.null(s)){
    return(NA)
  } else if (is.na(s)) {
    return(NA)
  }
  ceiling(b + (s / a)^(1 / c)) / 100
}

JT_women_mark <- function(s, a = 15.9803, b = 3.8, c = 1.04){
  if (is.null(s)) {
    return(NA)
  } else if (is.na(s)) {
    return(NA)
  }
  ceiling((b + (s / a)^(1 / c)) * 100) / 100
}

`800m_women_mark` <- function(s, a = 0.11193, b = 254, c = 1.88){
  if (is.null(s)) {
    return(NA)
  } else if(is.na(s)) {
    return(NA)
  }
  if (s > 3715) {
    message("800m score must be <= 3715 points to receive a valid (positive) time")
    return(0)
  } else {
    floor((b - (s / a)^(1 / c)) * 100) / 100
  }
}

`100m_women_mark` <- function(s, a = 17.8570, b = 21, c = 1.81){
  if (is.null(s)) {
    return(NA)
  } else if (is.na(s)) {
    return(NA)
  }
  if (s > 4412) {
    message("100m score must be <= 4412 points to receive a valid (positive) time")
    return(0)
  } else {
    floor((b - (s / a)^(1 / c)) * 100) / 100
  }
}

DT_women_mark <- function(s, a = 12.3311, b = 3, c = 1.1){
  if (is.null(s)) {
    return(NA)
  } else if (is.na(s)) {
    return(NA)
  }
  ceiling((b + (s / a)^(1 / c)) * 100) / 100
}

PV_women_mark <- function(s, a = 0.44125, b = 100, c = 1.35){
  if (is.null(s)) {
    return(NA)
  } else if (is.na(s)) {
    return(NA)
  }
  ceiling(b + (s / a)^(1 / c)) / 100
}

`400m_women_mark` <- function(s, a = 1.34285, b = 91.7, c = 1.81){
  if (is.null(s)) {
    return(NA)
  } else if (is.na(s)) {
    return(NA)
  }
  if (s > 4784) {
    message("400m score must be <= 4784 points to receive a valid (positive) time")
    return(0)
  } else {
    floor((b - (s / a)^(1 / c)) * 100) / 100
  }
}

`1500m_women_mark` <- function(s, a = 0.02883, b = 535, c = 1.88){
  if (is.null(s)) {
    return(NA)
  } else if (is.na(s)) {
    return(NA)
  }
  if (s > 3882) {
    message("1500m score must be <= 3882 points to receive a valid (positive) time")
    return(0)
  } else {
    floor((b - (s / a)^(1 / c)) * 100) / 100
  }
}

`60mH_women_mark` <- function(s, a = 20.0479, b = 17, c = 1.835){
  if (is.null(s)) {
    return(NA)
  } else if (is.na(s)) {
    return(NA)
  }
  if (s > 3626) {
    message("60mH score must be <= 3626 points to receive a valid (positive) time")
    return(0)
  } else {
    floor((b - (s / a)^(1 / c)) * 100) / 100
  }
}
