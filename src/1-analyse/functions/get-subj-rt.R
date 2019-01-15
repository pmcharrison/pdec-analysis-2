get_subj_rt <- function(dat) {
  dat %>% 
    filter(cond == "randreg" & response == "hit") %>% 
    group_by(alphabet_size, tone_len_ms) %>% 
    summarise(mean = mean(rt_norm_tones_from_repeat),
              sd = sd(rt_norm_tones_from_repeat),
              n = n(), 
              se = sd / sqrt(n),
              ci_95_min = mean - 1.96 * se,
              ci_95_max = mean + 1.96 * se) %>% 
    ungroup()
}
