dec_men_mark <- function(seconds, `100m` = NA, LJ = NA, SP = NA, HJ = NA,
                         `400m` = NA, `110mH` = NA, DT = NA, PV = NA,
                         JT = NA, `1500m` = NA){
  scores <- list(`100m`, LJ, SP, HJ,
                 `400m`, `110mH`, DT, PV,
                 JT, `1500m`)
  marks <- mapply(rlang::exec, c(`100m_men_mark`, LJ_men_mark, SP_men_mark, HJ_men_mark,
                                 `400m_men_mark`, `110mH_men_mark`, DT_men_mark, PV_men_mark,
                                 JT_men_mark, `1500m_men_mark`), scores)
  scores <- mapply(rlang::exec, c(`100m_men`, LJ_men, SP_men, HJ_men,
                                  `400m_men`, `110mH_men`, DT_men, PV_men, JT_men, `1500m_men`), marks)
  event_names <- c("100m", "LJ", "SP", "HJ", "400m",
                   "110mH", "DT", "PV", "JT", "1500m")
  combined_events(marks, scores, event_names, "decathlon", seconds)
}

penta_men_out_mark <- function(seconds, LJ = NA, JT = NA, `200m` = NA, DT = NA,
                               `1500m` = NA){
  scores <- list(LJ, JT, `200m`, DT, `1500m`)
  marks <- mapply(rlang::exec, c(LJ_men_mark, JT_men_mark, `200m_men_mark`,
                                 DT_men_mark, `1500m_men_mark`), scores)
  scores <- mapply(rlang::exec, c(LJ_men, JT_men, `200m_men`,
                                  DT_men, `1500m_men`), marks)
  event_names <- c("LJ", "JT", "200m", "DT", "1500m")
  combined_events(marks, scores, event_names, "outdoor_pentathlon", seconds)
}

hept_men_mark <- function(seconds, `60m` = NA, LJ = NA, SP = NA, HJ = NA,
                          `60mH` = NA, PV = NA, `1000m` = NA){
  scores <- list(`60m`, LJ, SP, HJ, `60mH`, PV, `1000m`)
  marks <- mapply(rlang::exec, c(`60m_men_mark`, LJ_men_mark, SP_men_mark, HJ_men_mark,
                                 `60mH_men_mark`, PV_men_mark, `1000m_men_mark`), scores)
  scores <- mapply(rlang::exec, c(`60m_men`, LJ_men, SP_men, HJ_men,
                                  `60mH_men`, PV_men, `1000m_men`), marks)
  event_names <- c("60m", "LJ", "SP", "HJ", "60mH", "PV", "1000m")
  combined_events(marks, scores, event_names, "heptathlon", seconds)
}

penta_men_in_mark <- function(seconds, `60mH` = NA, LJ = NA, SP = NA, HJ = NA,
                              `1000m` = NA){
  scores <- list(`60mH`, LJ, SP, HJ, `1000m`)
  marks <- mapply(rlang::exec, c(`60mH_men_mark`, LJ_men_mark, SP_men_mark,
                                 HJ_men_mark, `1000m_men_mark`), scores)
  scores <- mapply(rlang::exec, c(`60mH_men`, LJ_men, SP_men,
                                  HJ_men, `1000m_men`), marks)
  event_names <- c("60mH", "LJ", "SP", "HJ", "1000m")
  combined_events(marks, scores, event_names, "indoor_pentathlon", seconds)
}

hept_women_mark <- function(seconds, `100mH` = NA, HJ = NA, SP = NA, `200m` = NA,
                            LJ = NA, JT = NA, `800m` = NA){
  scores <- list(`100mH`, HJ, SP, `200m`, LJ, JT, `800m`)
  marks <- mapply(rlang::exec, c(`100mH_women_mark`, HJ_women_mark, SP_women_mark,
                                 `200m_women_mark`, `LJ_women_mark`, JT_women_mark,
                                 `800m_women_mark`), scores)
  scores <- mapply(rlang::exec, c(`100mH_women`, HJ_women, SP_women,
                                  `200m_women`, `LJ_women`, JT_women, `800m_women`), marks)
  event_names <- c("100mH", "HJ", "SP", "200m", "LJ", "JT", "800m")
  combined_events(marks, scores, event_names, "heptathlon", seconds)
}

dec_women_mark <- function(seconds, `100m` = NA, DT = NA, PV = NA, JT = NA,
                           `400m` = NA, `100mH` = NA, LJ = NA, SP = NA,
                           HJ = NA, `1500m` = NA){
  scores <- list(`100m`, DT, PV, JT, `400m`, `100mH`, LJ, SP,
                 HJ, `1500m`)
  marks <- mapply(rlang::exec, c(`100m_women_mark`, DT_women_mark, PV_women_mark, JT_women_mark,
                                 `400m_women_mark`, `100mH_women_mark`, LJ_women_mark,
                                 SP_women_mark, HJ_women_mark, `1500m_women_mark`), scores)
  scores <- mapply(rlang::exec, c(`100m_women`, DT_women, PV_women, JT_women,
                                  `400m_women`, `100mH_women`, LJ_women,
                                  SP_women, HJ_women, `1500m_women`), marks)
  event_names <- c("100m", "DT", "PV", "JT", "400m",
                   "100mH", "LJ", "SP", "HJ", "1500m")
  combined_events(marks, scores, event_names, "decathlon", seconds)
}

penta_women_mark <- function(seconds, `60mH` = NA, HJ = NA, SP = NA, LJ = NA,
                             `800m` = NA){
  scores <- list(`60mH`, HJ, SP, LJ, `800m`)
  marks <- mapply(rlang::exec, c(`60mH_women_mark`, HJ_women_mark, SP_women_mark,
                                 LJ_women_mark, `800m_women_mark`), scores)
  scores <- mapply(rlang::exec, c(`60mH_women`, HJ_women, SP_women,
                                  LJ_women, `800m_women`), marks)
  event_names <- c("60mH", "HJ", "SP", "LJ", "800m")
  combined_events(marks, scores, event_names, "pentathlon", seconds)
}
