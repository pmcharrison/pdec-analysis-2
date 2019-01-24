summarise_models <- function(y) {
  res <- map(seq_len(nrow(y)),
      ~ cbind(y[., ] %>% select(- res),
              y$res[[.]]) %>% as_tibble) %>% 
    bind_rows() %>% 
    select(- detail) %>% 
    mutate(
      error_count = map_int(lag_tones, ~ sum(is_lag_invalid(.))),
      mean = map_dbl(lag_tones, ~ mean(exclude_invalid_lags(.))),
      sd = map_dbl(lag_tones, ~ sd(exclude_invalid_lags(.))),
      n = map_int(lag_tones, ~ length(exclude_invalid_lags(.))),
      se = sd / sqrt(n),
      ci_95_min = mean - qnorm(0.975) * se,
      ci_95_max = mean + qnorm(0.975) * se
    ) 
  if (any(res$error_count > 0)) 
    warning("errors found in change-point detection")
  res
}

is_lag_invalid <- function(x) {
  is.na(x) | x <= 0 | x >= 50
}

exclude_invalid_lags <- function(x) {
  x[!is_lag_invalid(x)]
}
