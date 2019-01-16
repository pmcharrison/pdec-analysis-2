get_ppm_spec <- function() {
  tribble(
    ~ label,             ~ buffer_len, ~ stm_half_life, ~ stm_rate, ~ ltm_rate, ~ noise,
    "original",          Inf,            Inf,           1,            0,        0,
    "stm_rate",          Inf,            Inf,           0.1,          0,        0,
    "stm_rate_noise",    Inf,            Inf,           0.1,          0,        1,
    "decay",             0,              0.02,          0.1,          0,        1,
    "decay_plus_buffer", 15,             0.02,          1,            0.01,     1,
  ) %>% 
    mutate(i = seq_len(n())) %>% 
    select(i, everything())
}

ppm_options_from_ppm_spec <- function(x) {
  if (is.data.frame(x)) stopifnot(nrow(x) == 1L)
  PPMdecay::ppm_options(
    decay = buffer_decay_fun(buffer_len = x$buffer_len,
                             stm_half_life = x$stm_half_life,
                             stm_rate = x$stm_rate,
                             ltm_rate = x$ltm_rate,
                             noise = x$noise)
  )
}

buffer_decay_fun <- function(buffer_len, stm_half_life, stm_rate, ltm_rate, noise) {
  PPMdecay::decay_fun(function(pos, time, data_pos, data_time, all_time, ...) {
    if (any(data_time > time)) 
      stop("tried to predict using training data from the future")
    if (any(data_pos < 1))
      stop("data_pos cannot be less than 0")
    n <- length(data_pos)
    data_buffer_fail_time = all_time[data_pos + buffer_len]
    data_time_since_buffer_fail = time - data_buffer_fail_time
    weights <- if_else(is.na(data_time_since_buffer_fail) |
                         data_time_since_buffer_fail < 0,
                       stm_rate,
                       decay_exp(data_time_since_buffer_fail, 
                                 stm_half_life, stm_rate, ltm_rate))
    pmax(0, sum(weights) + rnorm(n = 1, sd = noise))
  })
}

decay_exp <- function(time_elapsed, half_life, start, end) {
  lambda <- log(2) / half_life
  end + (start - end) * exp(- lambda * time_elapsed)
}
