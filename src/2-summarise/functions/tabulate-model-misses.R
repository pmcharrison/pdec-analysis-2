tabulate_model_misses <- function(x) {
  map2(x$group, x$res, function(group, res) {
    res %>% 
      transmute(alphabet_size = alphabet_size,
                tone_len_ms = tone_len_ms,
                n = map_int(lag_tones, length),
                min_lag = map_int(lag_tones, min),
                max_lag = map_int(lag_tones, max),
                n_lag_too_small = map_int(lag_tones, ~ sum(. < 0)),
                prop_lag_too_small = n_lag_too_small / n,
                n_lag_too_big = map_int(lag_tones, ~ sum(. >= 50)),
                prop_lag_too_big = n_lag_too_big / n) %>% 
      add_column(group = group, .before = 1) %>% 
      mutate(group = gsub("`", "", group, fixed = TRUE))
  }) %>% 
    bind_rows()
}
