library(tidyverse)

tidy_all <- function() {
  read_delim("input/response/all.csv", 
             delim = "\t", 
             col_types = cols(subj = col_integer(),
                              ` block` = col_integer(),
                              ` speed` = col_integer(),
                              ` trialN` = col_integer(),
                              ` condi` = col_integer(),
                              ` setID` = col_integer(),
                              ` correct` = col_integer())) %>% 
    rename_all(~ gsub(" ", "", .)) %>% 
    rename(speed_i = speed,
           cond_i = condi,
           d_prime = dprime,
           trial_n = trialN,
           set_id = setID,
           rt = RTs,
           include = correct) %>% 
    mutate(
      d_prime = as.numeric(gsub(" ", "", d_prime)),
      alphabet_size = if_else(speed_i < 4, 10L, 20L),
      cond = recode_factor(cond_i, 
                           `1` = "rand",
                           `2` = "randreg",
                           `3` = "cont",
                           `4` = "step"),
      response = recode_factor(response,
                               `-2` = "too_early",
                               `-1` = "false_alarm",
                               `0` = "miss",
                               `1` = "hit",
                               `2` = "correct_rejection"),
      tone_len_ms = recode(speed_i,
                           `1` = 25,
                           `2` = 50,
                           `3` = 75,
                           `4` = 25,
                           `5` = 50,
                           `6` = 75),
      include = as.logical(include),
    )
}
