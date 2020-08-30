# Men's decathlon at the 2016 Summer Olympics
library(rvest)
library(tidyr)
library(dplyr)
library(textclean)

rio_dec <- read_html("https://en.wikipedia.org/wiki/Athletics_at_the_2016_Summer_Olympics_%E2%80%93_Men%27s_decathlon")

tbls <- rio_dec %>%
  html_nodes("table")

dec <- tbls[[19]] %>%
  html_table()

names(dec) <- c("rank", "athlete", "nationality",
                "score_total", "100m", "LJ", "SP", "HJ", "400m",
                "110mH", "DT", "PV", "JT", "1500m")

dec$rank[1:3] <- 1:3
dec$rank <- na_if(dec$rank, "n/a") %>%
  as.numeric()

dec <- separate(dec, score_total, c("score_total", NA), sep = 4)

# separating event columns
dec <- dec %>%
  separate("100m", c("100m_p", "100m"), sep = "(?=\\d{2}[.])") %>%
  separate(LJ, c("LJ_p", "LJ"), sep = "(?=\\d{1}[.])",
           convert = TRUE) %>%
  separate(SP, c("SP_p", "SP"), sep = "(?=\\d{2}[.])|(?=NM)") %>%
  separate(HJ, c("HJ_p", "HJ"), sep = 3, convert = TRUE) %>%
  separate(`400m`, c("400m_p", "400m"), sep = "(?=\\d{2}[.])",
           convert = TRUE) %>%
  separate(`110mH`, c("110mH_p", "110mH"), sep = "(?=\\d{2}[.])",
           convert = TRUE) %>%
  separate(DT, c("DT_p", "DT"), sep = "(?=\\d{2}[.])|(?=NM)") %>%
  separate(PV, c("PV_p", "PV"), sep = "(?=\\d{1}[.])|(?=NM)") %>%
  separate(JT, c("JT_p", "JT"), sep = "(?=\\d{2}[.])",
           convert = TRUE) %>%
  separate(`1500m`, c("1500m_p", "1500m"),
           sep = "(?=\\d{1}\\:)|(?=DNF)")

# reordering columns
dec <- dec[, c(1:4, as.vector(rbind(seq(6, 24, 2), seq(5, 23, 2))))]

# no Did Not Finish (DNF), Did Not Start (DNS), or No Mark (NM)
dec <- dec[1:23,]

dec <- dec %>%
  mutate(across(c(ends_with("p", ignore.case = FALSE), score_total), as.integer))
dec <- dec %>%
  mutate(across(c(`100m`, SP, DT, PV), as.double))

# replace non-ASCII characters
dec <- dec %>%
  mutate(athlete = replace_non_ascii(athlete))

usethis::use_data(dec, overwrite = TRUE)
