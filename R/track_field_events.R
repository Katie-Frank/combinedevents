`100m_men` <- function(x, a = 25.4347, b = 18, c = 1.81){
  if (is.null(x)) {
    return(NA)
  } else if (is.na(x)) {
    return(NA)
  }
  if (x > 17.83) {
    message('100m time must be <= 17.83 seconds to receive a positive score')
    return(0)
  } else {
    as.integer(a * (b - x)^c)
  }
}

LJ_men <- function(x, a = 0.14354, b = 220, c = 1.4){
  if (is.null(x)) {
    return(NA)
  } else if (is.na(x)) {
    return(NA)
  }
  if (x < 2.25) {
    message('LJ distance must be >= 2.25m to receive a positive score')
    return(0)
  }
  as.integer(a * (100 * x - b)^c) # P is in cm
}

SP_men <- function(x, a = 51.39, b = 1.5, c = 1.05){
  if (is.null(x)) {
    return(NA)
  } else if (is.na(x)) {
    return(NA)
  }
  if (x < 1.53) {
    message("SP distance must be >= 1.53m to receive a positive score")
    return(0)
  }
  as.integer(a * (x - b)^c)
}

HJ_men <- function(x, a = 0.8465, b = 75, c = 1.42){
  if (is.null(x)) {
    return(NA)
  } else if (is.na(x)) {
    return(NA)
  }
  if (x < 0.77) {
    message("HJ height must be >= 0.77m to receive a positive score")
    return(0)
  }
  as.integer(a * (100 * x - b)^c)
}

`400m_men` <- function(x, a = 1.53775, b = 82, c = 1.81){
  if (is.null(x)) {
    return(NA)
  } else if (is.na(x)) {
    return(NA)
  }
  if (x > 81.21) {
    message("400m time must be <= 81.21 seconds to receive a positive score")
    return(0)
  }
  as.integer(a * (b - x)^c)
}

`110mH_men` <- function(x, a = 5.74352, b = 28.5 , c = 1.92){
  if (is.null(x)) {
    return(NA)
  } else if (is.na(x)) {
    return(NA)
  }
  if (x > 28.09) {
    message("110mH time must be <= 28.09 seconds to receive a positive score")
    return(0)
  }
  as.integer(a * (b - x)^c)
}


DT_men <- function(x, a = 12.91, b = 4, c = 1.1){
  if (is.null(x)) {
    return(NA)
  } else if (is.na(x)) {
    return(NA)
  }
  if (x < 4.10) {
    message("DT distance must be >= 4.10m to receive a positive score")
    return(0)
  }
  as.integer(a * (x - b)^c)
}

PV_men <- function(x, a = 0.2797, b = 100, c = 1.35){
  if (is.null(x)) {
    return(NA)
  } else if (is.na(x)) {
    return(NA)
  }
  if (x < 1.03) {
    message("PV height must be >= 1.03m to receive a positive score")
    return(0)
  }
  as.integer(a * (100 * x - b)^c)
}

JT_men <- function(x, a = 10.14, b = 7, c = 1.08){
  if (is.null(x)) {
    return(NA)
  } else if(is.na(x)) {
    return(NA)
  }
  if (x < 7.12) {
    message("JT distance must be >= 7.12m to receive a positive score")
    return(0)
  }
  as.integer(a * (x - b)^c)
}

`1500m_men` <- function(x, a = 0.03768, b = 480, c = 1.85){
  if (is.null(x)) {
    return(NA)
  } else if (is.na(x)) {
    return(NA)
  }
  if (x > 474.11) {
    message("1500m time must be <= 7:54.11 minutes to receive a positive score")
    return(0)
  }
  as.integer(a * (b - x)^c)
}

`200m_men` <- function(x, a = 5.8425, b = 38, c = 1.81){
  if (is.null(x)) {
    return(NA)
  } else if (is.na(x)) {
    return(NA)
  }
  if (x > 37.62) {
    message('200m time must be <= 37.62 seconds to receive a positive score')
    return(0)
  } else {
    as.integer(a * (b - x)^c)
  }
}

`60m_men` <- function(x, a = 58.0150, b = 11.5, c = 1.81){
  if (is.null(x)) {
    return(NA)
  } else if (is.na(x)) {
    return(NA)
  }
  if (x > 11.39) {
    message('60m time must be <= 11.39 seconds to receive a positive score')
    return(0)
  } else {
    as.integer(a * (b - x)^c)
  }
}

`60mH_men` <- function(x, a = 20.5173, b = 15.5, c = 1.92){
  if (is.null(x)) {
    return(NA)
  } else if (is.na(x)) {
    return(NA)
  }
  if (x > 15.29) {
    message('60mH time must be <= 15.29 seconds to receive a positive score')
    return(0)
  } else {
    as.integer(a * (b - x)^c)
  }
}

`1000m_men` <- function(x, a = 0.08713, b = 305.5, c = 1.85){
  if (is.null(x)) {
    return(NA)
  } else if (is.na(x)) {
    return(NA)
  }
  if (x > 301.75) {
    message("1000m time must be <= 5:01.75 minutes to receive a positive score")
    return(0)
  } else {
    as.integer(a * (b - x)^c)
  }
}

`100mH_women` <- function(x, a = 9.23076, b = 26.7, c = 1.835){
  if (is.null(x)) {
    return(NA)
  } else if (is.na(x)) {
    return(NA)
  }
  if (x > 26.4) {
    message("100mH time must be <= 26.40 seconds to receive a positive score")
    return(0)
  }
  as.integer(a * (b - x)^c)
}

HJ_women <- function(x, a = 1.84523, b = 75, c = 1.348){
  if (is.null(x)) {
    return(NA)
  } else if (is.na(x)) {
    return(NA)
  }
  if (x < 0.76) {
    message("HJ height must be >= 0.76m to receive a positive score")
    return(0)
  }
  as.integer(a * (100 * x - b)^c)
}

SP_women <- function(x, a = 56.0211, b = 1.5, c = 1.05){
  if (is.null(x)) {
    return(NA)
  } else if (is.na(x)) {
    return(NA)
  }
  if (x < 1.53) {
    message("SP distance must be >= 1.53m to receive a positive score")
    return(0)
  }
  as.integer(a * (x - b)^c)
}

`200m_women` <- function(x, a = 4.99087, b = 42.50, c = 1.81){
  if (is.null(x)) {
    return(NA)
  } else if (is.na(x)) {
    return(NA)
  }
  if (x > 42.08) {
    message('200m time must be <= 42.08 seconds to receive a positive score')
    return(0)
  } else {
    as.integer(a * (b - x)^c)
  }
}

LJ_women <- function(x, a = 0.188807, b = 210, c = 1.41){
  if (is.null(x)){
    return(NA)
  } else if (is.na(x)) {
    return(NA)
  }
  if (x < 2.14) {
    message('LJ distance must be >= 2.14m to receive a positive score')
    return(0)
  }
  as.integer(a * (100 * x - b)^c)
}

JT_women <- function(x, a = 15.9803, b = 3.8, c = 1.04){
  if (is.null(x)) {
    return(NA)
  } else if (is.na(x)) {
    return(NA)
  }
  if (x < 3.87) {
    message("JT distance must be >= 3.87m to receive a positive score")
    return(0)
  }
  as.integer(a * (x - b)^c)
}

`800m_women` <- function(x, a = 0.11193, b = 254, c = 1.88){
  if (is.null(x)) {
    return(NA)
  } else if(is.na(x)) {
    return(NA)
  }
  if (x > 250.79) {
    message("800m time must be <= 4:10.79 minutes to receive a positive score")
    return(0)
  }
  as.integer(a * (b - x)^c)
}

`100m_women` <- function(x, a = 17.8570, b = 21, c = 1.81){
  if (is.null(x)) {
    return(NA)
  } else if (is.na(x)) {
    return(NA)
  }
  if (x > 20.79) {
    message('100m time must be <= 20.79 seconds to receive a positive score')
    return(0)
  } else {
    as.integer(a * (b - x)^c)
  }
}

DT_women <- function(x, a = 12.3311, b = 3, c = 1.1){
  if (is.null(x)) {
    return(NA)
  } else if (is.na(x)) {
    return(NA)
  }
  if (x < 3.11) {
    message("DT distance must be >= 3.11m to receive a positive score")
    return(0)
  }
  as.integer(a * (x - b)^c)
}

PV_women <- function(x, a = 0.44125, b = 100, c = 1.35){
  if (is.null(x)) {
    return(NA)
  } else if (is.na(x)) {
    return(NA)
  }
  if (x < 1.02) {
    message("PV height must be >= 1.02m to receive positive score")
    return(0)
  }
  as.integer(a * (100 * x - b)^c)
}

`400m_women` <- function(x, a = 1.34285, b = 91.7, c = 1.81){
  if (is.null(x)) {
    return(NA)
  } else if (is.na(x)) {
    return(NA)
  }
  if (x > 90.85) {
    message("400m time must be <= 90.85 seconds to receive a positive score")
    return(0)
  }
  as.integer(a * (b - x)^c)
}

`1500m_women` <- function(x, a = 0.02883, b = 535, c = 1.88){
  if (is.null(x)) {
    return(NA)
  } else if (is.na(x)) {
    return(NA)
  }
  if (x > 528.4) {
    message("1500m time must be <= 8:48.40 minutes to receive a positive score")
    return(0)
  }
  as.integer(a * (b - x)^c)
}

`60mH_women` <- function(x, a = 20.0479, b = 17, c = 1.835){
  if (is.null(x)) {
    return(NA)
  } else if (is.na(x)) {
    return(NA)
  }
  if (x > 16.8) {
    message('60mH time must be <= 16.80 seconds to receive a positive score')
    return(0)
  } else {
    as.integer(a * (b - x)^c)
  }
}
