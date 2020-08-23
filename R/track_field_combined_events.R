dec_men <- function(seconds, `100m` = NA, LJ = NA, SP = NA, HJ = NA,
                    `400m` = NA, `110mH` = NA, DT = NA, PV = NA,
                    JT = NA, `1500m` = NA){
  marks <- list(`100m`, LJ, SP, HJ,
                `400m`, `110mH`, DT, PV,
                JT, `1500m`)
  scores <- mapply(rlang::exec, c(`100m_men`, LJ_men, SP_men, HJ_men,
                                  `400m_men`, `110mH_men`, DT_men, PV_men, JT_men, `1500m_men`), marks)
  event_names <- c("100m", "LJ", "SP", "HJ", "400m",
                   "110mH", "DT", "PV", "JT", "1500m")
  combined_events(marks, scores, event_names, "decathlon", seconds)
}

penta_men_out <- function(seconds, LJ = NA, JT = NA, `200m` = NA, DT = NA,
                          `1500m` = NA){
  marks <- list(LJ, JT, `200m`, DT, `1500m`)
  scores <- mapply(rlang::exec, c(LJ_men, JT_men, `200m_men`,
                                  DT_men, `1500m_men`), marks)
  event_names <- c("LJ", "JT", "200m", "DT", "1500m")
  combined_events(marks, scores, event_names, "outdoor_pentathlon", seconds)
}

hept_men <- function(seconds, `60m` = NA, LJ = NA, SP = NA, HJ = NA,
                     `60mH` = NA, PV = NA, `1000m` = NA){
  marks <- list(`60m`, LJ, SP, HJ, `60mH`, PV, `1000m`)
  scores <- mapply(rlang::exec, c(`60m_men`, LJ_men, SP_men, HJ_men,
                                  `60mH_men`, PV_men, `1000m_men`), marks)
  event_names <- c("60m", "LJ", "SP", "HJ", "60mH", "PV", "1000m")
  combined_events(marks, scores, event_names, "heptathlon", seconds)
}

penta_men_in <- function(seconds, `60mH` = NA, LJ = NA, SP = NA, HJ = NA,
                         `1000m` = NA){
  marks <- list(`60mH`, LJ, SP, HJ, `1000m`)
  scores <- mapply(rlang::exec, c(`60mH_men`, LJ_men, SP_men,
                                  HJ_men, `1000m_men`), marks)
  event_names <- c("60mH", "LJ", "SP", "HJ", "1000m")
  combined_events(marks, scores, event_names, "indoor_pentathlon", seconds)
}

hept_women <- function(seconds, `100mH` = NA, HJ = NA, SP = NA, `200m` = NA,
                       LJ = NA, JT = NA, `800m` = NA){
  marks <- list(`100mH`, HJ, SP, `200m`, LJ, JT, `800m`)
  scores <- mapply(rlang::exec, c(`100mH_women`, HJ_women, SP_women,
                                  `200m_women`, `LJ_women`, JT_women, `800m_women`), marks)
  event_names <- c("100mH", "HJ", "SP", "200m", "LJ", "JT", "800m")
  combined_events(marks, scores, event_names, "heptathlon", seconds)
}

dec_women <- function(seconds, `100m` = NA, DT = NA, PV = NA, JT = NA,
                      `400m` = NA, `100mH` = NA, LJ = NA, SP = NA,
                      HJ = NA, `1500m` = NA){
  marks <- list(`100m`, DT, PV, JT,
                `400m`, `100mH`, LJ, SP,
                HJ, `1500m`)
  scores <- mapply(rlang::exec, c(`100m_women`, DT_women, PV_women, JT_women,
                                  `400m_women`, `100mH_women`, LJ_women,
                                  SP_women, HJ_women, `1500m_women`), marks)
  event_names <- c("100m", "DT", "PV", "JT", "400m",
                   "100mH", "LJ", "SP", "HJ", "1500m")
  combined_events(marks, scores, event_names, "decathlon", seconds)
}

penta_women <- function(seconds, `60mH` = NA, HJ = NA, SP = NA, LJ = NA,
                        `800m` = NA){
  marks <- list(`60mH`, HJ, SP, LJ, `800m`)
  scores <- mapply(rlang::exec, c(`60mH_women`, HJ_women, SP_women,
                                  LJ_women, `800m_women`), marks)
  event_names <- c("60mH", "HJ", "SP", "LJ", "800m")
  combined_events(marks, scores, event_names, "pentathlon", seconds)
}
