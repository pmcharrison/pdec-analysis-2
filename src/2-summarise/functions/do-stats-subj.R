do_stats_subj <- function(dat_response) {
  raw <- dat_response %>% 
    filter(cond == "randreg" & response == "hit") %>% 
    group_by(subj, alphabet_size, tone_len_ms) %>% 
    summarise(rt_norm_tones_from_repeat = median(rt_norm_tones_from_repeat)) %>% 
    group_by(alphabet_size, tone_len_ms) %>% 
    do(data = (.)) %>% 
    ungroup() %>% 
    add_column(group = NA, .before = 1) %>% 
    mutate(group = seq_len(n()))
  
  gtools::combinations(n = nrow(raw), r = 2, v = raw$group) %>% 
    as.data.frame() %>% 
    set_names(c("group_1", "group_2")) %>% 
    as_tibble() %>% 
    mutate(dat_1 = map(group_1, ~ select(filter(raw, group == .)$data[[1]], 
                                         subj,
                                         rt_norm_tones_from_repeat)),
           dat_2 = map(group_2, ~ select(filter(raw, group == .)$data[[1]], 
                                         subj,
                                         rt_norm_tones_from_repeat)),
           dat = map2(dat_1, dat_2, inner_join, by = "subj")) %>% 
    select(- c(dat_1, dat_2)) %>% 
    mutate(
      wilcox = map(dat, ~ wilcox.test(x = .$rt_norm_tones_from_repeat.x,
                                      y = .$rt_norm_tones_from_repeat.y,
                                      paired = TRUE) %>% broom::glance()
      )) %>% select(- dat) %>% unnest() %>% 
    merge(select(raw, group, alphabet_size, tone_len_ms),
          by.x = "group_1", by.y = "group") %>% 
    merge(select(raw, group, alphabet_size, tone_len_ms),
          by.x = "group_2", by.y = "group",
          suffixes = c("_1", "_2")) %>% 
    as_tibble() %T>% 
    write_csv("output/stats-subj.csv")
}
