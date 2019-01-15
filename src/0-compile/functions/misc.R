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
  n <- nrow(dat)
  res <- filter(dat, include)
  message("Excluded ", n - nrow(res), " further trials")
  res
}

drop_excluded_subj <- function(dat, subj_to_exclude) {
  n <- nrow(dat)
  res <- filter(dat, !subj %in% subj_to_exclude)
  message("Excluded ", length(subj_to_exclude), " participant(s) with ",
          n - nrow(res), " trials")
  res
}

drop_supplementary <- function(dat) {
  dat$all
}

get_rt_baselines_1 <- function(dat) {
  dat %>% 
    group_by(subj, block) %>% 
    filter(cond == "step" & response == "hit" & include) %>% 
    summarise(rt_reference = mean(rt))
}

norm_reaction_times <- function(dat, rt_baselines_1) {
  dat <- left_join(dat, rt_baselines_1, by = c("subj", "block"))
  dat %>% mutate(
    rt_norm = if_else(cond == "randreg", rt - rt_reference, rt),
    rt_norm_tones = 1000 * rt_norm / tone_len_ms
  )
}

get_rt_baselines_2 <- function(dat) {
  dat %>% 
    group_by(tone_len_ms, alphabet_size, cond_i) %>% 
    summarise(rt_norm_reference_mean = mean(rt_norm, na.rm = TRUE),
              rt_norm_reference_sd = sd(rt_norm, na.rm = TRUE))
}

drop_outliers <- function(dat, rt_baselines_2, z_threshold) {
  n <- nrow(dat)
  dat <- left_join(dat, rt_baselines_2,
                   by = c("tone_len_ms", "alphabet_size", "cond_i"))
  stopifnot(n == nrow(dat))
  dat <- dat %>% mutate(
    rt_norm_z = (rt_norm - rt_norm_reference_mean) / rt_norm_reference_sd
  )
  dat <- filter(dat, is.na(rt_norm) | abs(rt_norm_z) < !! z_threshold)
  message("Dropped ", n - nrow(dat), " outliers")
  dat
}

regression_test <- function(dat) {
  x <- dat %>% 
    filter(cond_i %in% c(2, 4)) %>% 
    arrange(subj, block, trial_n) %>% 
    select(subj, block, trial_n, cond_i, rt, d_prime, rt_norm)
  y <- read.delim("archive/roberta-outputs/exp_stm.txt") %>% 
    as.tibble %>% 
    arrange(subj, block, trialN) %>% 
    select(subj, block, trialN, condi, RTs, dprime, RTadj) %>% 
    rename(trial_n = trialN, cond_i = condi, rt = RTs, d_prime = dprime, rt_norm = RTadj)
  expect_equal(as.data.frame(x), as.data.frame(y))
}
