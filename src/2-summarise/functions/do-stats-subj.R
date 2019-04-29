library(broom)

do_stats_subj <- function(subj_vals) {
  raw <- subj_vals %>% 
    group_by(alphabet_size, tone_len_ms) %>% 
    do(data = (.)) %>% 
    ungroup() %>% 
    add_column(group = NA, .before = 1) %>% 
    mutate(group = seq_len(n()))
  
  list(
    wilcox = wilcox_subj(raw),
    friedman = friedman_subj(raw)
  )
}

friedman_subj <- function(raw) {
  x <- raw$data %>% bind_rows() %>% split(., .$subj)
  map(c(`10` = 10, `20` = 20), function(alph_size) {
    map(x, function(subj_dat) {
      c(25, 50, 75) %>% set_names(., .) %>% 
        map(function(tone_len) {
          subj_dat %>% 
            filter(alphabet_size == alph_size & 
                     tone_len_ms == tone_len) %>%
            pull(subj_val)
        }) %>% as_tibble()
    }) %>% bind_rows() %>% as.matrix() %>% friedman.test() %>% glance() %>% 
      add_column(alphabet_size = alph_size, .before = 1)
  }) %>% bind_rows() %T>% write_csv("output/stats-subj-friedman.csv")
}

wilcox_subj <- function(raw) {
  gtools::combinations(n = nrow(raw), r = 2, v = raw$group) %>% 
    as.data.frame() %>% 
    set_names(c("group_1", "group_2")) %>% 
    as_tibble() %>% 
    mutate(dat_1 = map(group_1, ~ select(filter(raw, group == .)$data[[1]], 
                                         subj,
                                         subj_val)),
           dat_2 = map(group_2, ~ select(filter(raw, group == .)$data[[1]], 
                                         subj,
                                         subj_val)),
           dat = map2(dat_1, dat_2, inner_join, by = "subj")) %>% 
    select(- c(dat_1, dat_2)) %>% 
    mutate(
      wilcox = map(dat, ~ wilcox.test(x = .$subj_val.x,
                                      y = .$subj_val.y,
                                      paired = TRUE) %>% broom::glance()
      )) %>% select(- dat) %>% unnest() %>% 
    merge(select(raw, group, alphabet_size, tone_len_ms),
          by.x = "group_1", by.y = "group") %>% 
    merge(select(raw, group, alphabet_size, tone_len_ms),
          by.x = "group_2", by.y = "group",
          suffixes = c("_1", "_2")) %>% 
    as_tibble() %T>% 
    write_csv("output/stats-subj-wilcox.csv")
}
