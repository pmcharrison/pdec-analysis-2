get_ppm_spec <- function() {
  read_csv("input/ppm-spec.csv", col_types = cols()) %>% 
    filter(include) %>% 
    mutate(i = seq_len(n())) %>% 
    select(- include) %>% 
    select(i, everything())
}

# "Original PPM",              Inf,            Inf,             1,          0,          0,
# "Instant buffer decay",      18,            0.0000001,          1,        0.3,        0.5,
# "Gradual buffer decay (V)",  18,            0.04,               1,        0.35,       0.5,
# "Gradual buffer decay (VI)", 18,            0.04,               1,        0.5,        0.5
# "Gradual buffer decay (I)",  15,            0.1,                1,         0.3,        0.5,
# "Gradual buffer decay (II)", 15,            0.1,                1,          0.35,        0.5,
# "Gradual buffer decay (III)",15,            0.08,               1,          0.35,        0.5,
# "Gradual buffer decay (IV)", 15,            0.04,               1,          0.35,        0.5,
# "Gradual buffer decay (7)",  17,            0.04,               1,          0.42,        0.5,
# "Gradual buffer decay (8)",  16,            0.04,               1,          0.42,        0.5,
# "Gradual buffer decay (9)",  15,            0.04,               1,          0.42,        0.5,
# "Gradual buffer decay (10)", 14,            0.04,               1,          0.42,        0.5,
# "Gradual buffer decay (11)", 18,            0.04,               1,          0.45,        0.5,
# "Gradual buffer decay (12)", 18,            0.04,               1,          0.4,        0.5,
# "Gradual buffer decay (13)", 18,            0.04,               1,          0.35,        0.5,
# "Gradual buffer decay (VI)", 18,            0.125,              1,          0.35,        0.5
# "PPM1: Original",        Inf,            Inf,             1,          0,          0,
# "PPM2: + Noise",         Inf,            Inf,             1,          0,          0.75,
# "PPM3: + Memory decay",  0,            0.15,               1,          0,        0.75,
# "X",                     15,            0.001,             1,          0.7,        1,
# "stm_rate",          Inf,            Inf,           0.1,          0,        0,
# "stm_rate_noise",    Inf,            Inf,           0.1,          0,        1,
# "decay",             0,              0.5,           1,            0,        1,
# "decay_plus_buffer", 15,             0.5,           1,            0,        1,

ppm_options_from_ppm_spec <- function(x) {
  if (is.data.frame(x)) stopifnot(nrow(x) == 1L)
  stopifnot(x$order_bound <= 10L)
  # PPMdecay::ppm_options(
  #   PPMdecay::decay_buffer(buffer_time = x$buffer_time,
  #                          buffer_items = x$buffer_items,
  #                          buffer_rate = x$buffer_rate,
  #                          stm_half_life = x$stm_half_life,
  #                          stm_rate = x$stm_rate,
  #                          ltm_rate = x$ltm_rate,
  #                          noise = x$noise),
  #   order_bound = x$order_bound
  # )
  x
}
