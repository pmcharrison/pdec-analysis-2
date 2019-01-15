get_ppm_spec <- function() {
  tribble(
    ~ label,             ~ buffer_len, ~ stm_half_life, ~ stm_rate, ~ ltm_rate,
    "original",          Inf,            Inf,           1,          0,
    "decay",             0,              5,             1,          0,
    "decay_plus_buffer", 15,             5,             1,          0
  ) %>% mutate(i = seq_len(n())) %>% 
    select(i, everything()) %>% 
    mutate(
      decay_fun = pmap(buffer_len, stm_half_life, stm_rate, ltm_rate)
    )
}


buffer_decay_fun <- function(buffer_len, stm_half_life, stm_rate, ltm_rate) {
  PPMdecay::decay_fun(function(pos, time, data_pos, data_time, all_time) {
    if (any(data_time > time))
      stop("tried to predict using training data from the future")
    data_buffer_fail_time = all_time[data_pos + buffer_len],
    data_time_since_buffer_fail = time - data_buffer_fail_time
    ifelse(is.na(data_time_since_buffer_fail) ||
             data_time_since_buffer_fail < 0,
           stm_rate,
           decay_exp(data_time_since_buffer_fail, 
                     stm_half_life, stm_rate, ltm_rate))
    
  })
}

decay_exp <- function(time_elapsed, half_life, start, end) {
  lambda <- log(2) / half_life
  end + (start - end) * exp(- lambda * time_elapsed))
}
