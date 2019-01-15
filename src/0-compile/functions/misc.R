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
  message("Excluded ", nrow(res) - n, " trials")
}

drop_excluded_subj <- function(dat, subj_to_exclude) {
  n <- nrow(dat)
  res <- filter(dat, !subj %in% subj_to_exclude)
  message("Excluded ", length(subj_to_exclude), ", ", nrow(res) - n, " trials")
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
  dat <- left_join(dat, rt_baselines, by = c("subj", "block"))
  dat %>% mutate(
    rt_norm = rt - rt_reference,
    rt_norm_tones = 1000 * rt_norm / tone_len_ms
  )
}

get_rt_baselines_2 <- function(dat) {
  dat %>% 
    group_by(block, tone_len_ms, alphabet_size, cond_i) %>% 
    summarise(mean = mean(rt_norm, na.rm = TRUE),
              sd = sd(rt_norm, na.rm = TRUE))
}
