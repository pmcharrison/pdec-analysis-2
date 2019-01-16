summarise_models <- function(y) {
  res <- map(seq_len(nrow(y)),
      ~ cbind(y[., ] %>% select(- res),
              y$res[[.]]) %>% as.tibble) %>% 
    bind_rows() %>% 
    select(- detail) %>% 
    mutate(
      error_count = map_int(lag_tones, ~ sum(. <= 0 | . >= 10)),
      mean = map_dbl(lag_tones, mean),
      sd = map_dbl(lag_tones, sd),
      n = map_int(lag_tones, length),
      se = sd / sqrt(n),
      ci_95_min = mean - qnorm(0.975) * se,
      ci_95_max = mean + qnorm(0.975) * se
    ) 
  if (any(res$error_count > 0)) 
    warning("errors found in change-point detection")
  res
}
