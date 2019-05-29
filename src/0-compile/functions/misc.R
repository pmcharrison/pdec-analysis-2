get_raw <- function() {
  list(all = tidy_all(), 
       stim = tidy_stim(), 
       stim_desc = tidy_stim_desc())
}

merge_in_stim_desc <- function(dat) {
  dat$all$stim <- dat$stim_desc %>% 
    {split(dat$stim_desc, cumsum(c(TRUE, diff(.$tone_num) < 0)))}
  dat$all <- add_column(dat$all, 
                        transition = map_int(dat$all$stim, ~ unique(.$transition)), 
                        .before = "stim")
  dat
}

merge_in_stim <- function(dat) {
  dat$all$stim <- map2(dat$all$stim, do.call(c, dat$stim$data), function(x, y) {
    mutate(x, tone = y) %>% 
      select(tone_num, transition, tone)
  })
  dat
}

add_row_id <- function(dat) {
  add_column(dat, row_id = seq_len(nrow(dat)), .before = 1L)
}

drop_excluded_trials <- function(dat) {
  log <- "output/describe-exclude-trials.txt"
  n <- nrow(dat)
  write(paste0("Original number of trials: ", n), log)
  res <- filter(dat, include)
  msg <- paste0("Excluded ", n - nrow(res), " trials, ",
                "leaving ", nrow(res), " trials")
  write(msg, log, append = TRUE)
  message(msg)
  res
}

drop_excluded_subj <- function(dat, subj_to_exclude) {
  log <- "output/describe-subj.txt"
  n <- nrow(dat)
  write(paste0("Original number of subjects: ", length(unique(dat$subj))), log)
  write(paste0("Original number of trials: ", nrow(dat)), log, append = TRUE)
  write(paste0("Excluded subject IDs: ", paste(subj_to_exclude, collapse = ", ")),
        log, append = TRUE)
  res <- filter(dat, !subj %in% subj_to_exclude)
  msg <- paste0("Excluded ", length(subj_to_exclude), 
                " participant(s) with ",
                n - nrow(res), " trials, leaving ", nrow(res), " trials")
  message(msg)
  write(msg, log, append = TRUE)
  res
}

drop_supplementary <- function(dat) {
  dat$all
}

get_rt_baselines_1 <- function(dat) {
  dat %>% 
    # group_by(subj) %>%
    group_by(subj, block) %>%
    filter(cond == "step" & response == "hit" & include) %>% 
    summarise(rt_reference = mean(rt))
}

norm_reaction_times <- function(dat, rt_baselines_1) {
  # dat <- left_join(dat, rt_baselines_1, by = c("subj")) # used to join by block as well
  dat <- left_join(dat, rt_baselines_1, by = c("subj", "block")) # used to join by block as well
  dat %>% mutate(
    rt_norm = if_else(cond == "randreg", rt - rt_reference, rt),
    rt_norm_tones_from_transition = 1000 * rt_norm / tone_len_ms,
    rt_norm_tones_from_repeat = rt_norm_tones_from_transition - alphabet_size
  )
}

get_rt_baselines_2 <- function(dat) {
  dat %>% 
    group_by(tone_len_ms, alphabet_size, cond_i, subj) %>% 
    summarise(rt_norm_reference_mean = mean(rt_norm, na.rm = TRUE),
              rt_norm_reference_sd = sd(rt_norm, na.rm = TRUE))
}

drop_outliers <- function(dat, rt_baselines_2, z_threshold) {
  log <- "output/describe-drop-outliers.txt"
  n <- nrow(dat)
  write(paste0("Original number of trials: ", n), log)
  dat <- left_join(dat, rt_baselines_2,
                   by = c("tone_len_ms", "alphabet_size", "cond_i", "subj"))
  stopifnot(n == nrow(dat))
  dat <- dat %>% mutate(
    rt_norm_z = (rt_norm - rt_norm_reference_mean) / rt_norm_reference_sd
  )
  dat <- filter(dat, is.na(rt_norm) | abs(rt_norm_z) < !! z_threshold)
  msg <- paste0("Dropped ", n - nrow(dat), " outliers, leaving ", 
                nrow(dat), " trials")
  message(msg)
  write(msg, log, append = TRUE)
  dat
}

# This regression no longer works because we changed the outlier exclusion 
# criteria from Roberta's original analysis
regression_test <- function(dat) {
  x <- dat %>% 
    filter(cond_i %in% c(2, 4)) %>% 
    arrange(subj, block, trial_n) %>% 
    select(subj, block, trial_n, cond_i, rt, d_prime, rt_norm)
  y <- read.delim("archive/roberta-outputs/exp_stm.txt") %>% 
    as_tibble %>% 
    arrange(subj, block, trialN) %>% 
    select(subj, block, trialN, condi, RTs, dprime, RTadj) %>% 
    rename(trial_n = trialN, cond_i = condi, rt = RTs, d_prime = dprime, rt_norm = RTadj)
  expect_equal(as.data.frame(x), as.data.frame(y))
}
