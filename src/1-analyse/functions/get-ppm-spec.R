get_ppm_spec <- function() {
  read_csv("input/ppm-spec.csv", col_types = "cols")
  %>% mutate(i = seq_len(n())) %>% 
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
  PPMdecay::ppm_options(
    decay = buffer_decay_fun(buffer_len = x$buffer_len,
                             stm_half_life = x$stm_half_life,
                             stm_rate = x$stm_rate,
                             ltm_rate = x$ltm_rate,
                             noise = x$noise)
  )
}

buffer_decay_fun <- function(buffer_len, stm_half_life, stm_rate, ltm_rate, noise) {
  stopifnot(stm_half_life != 0)
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

library(testthat)
test_that("decay functions", {
  f <- buffer_decay_fun(buffer_len = 10, 
                        stm_half_life = 0.000000001, 
                        stm_rate = 1, 
                        ltm_rate = 0,
                        noise = 0)
  # Buffer = 10 - everything at full stm_rate
  f$fun(pos = 10, time = 10, 
        data_pos = 1:9, data_time = 1:9, 
        all_time = 1:9) %>% expect_equal(9)
  # No more than 10 cases can be counted
  f$fun(pos = 16, time = 16, 
        data_pos = 1:15, data_time = 1:15, 
        all_time = 1:15) %>% expect_equal(10)
  # Now set a non-zero ltm_rate
  f2 <- buffer_decay_fun(buffer_len = 10, 
                         stm_half_life = 0.000000001, 
                         stm_rate = 1, 
                         ltm_rate = 0.1,
                         noise = 0)
  f2$fun(pos = 16, time = 16, 
         data_pos = 1:15, data_time = 1:15, 
         all_time = 1:15) %>% expect_equal(10 + 0.5)
  # Now to distinguish time from position, 
  # we need to set a non-zero half-life.
  f3 <- buffer_decay_fun(buffer_len = 10, 
                         stm_half_life = 1, 
                         stm_rate = 1, 
                         ltm_rate = 0,
                         noise = 0)
  # Nothing within the buffer decays
  f3$fun(pos = 10, time = 10, 
         data_pos = 1:10, data_time = 1:10, 
         all_time = 1:10) %>% expect_equal(10)
  # Past the buffer, we decay with a half-life of 1
  f3$fun(pos = 12, time = 12, 
         data_pos = 1:11, data_time = 1:11, 
         all_time = 1:11) %>% expect_equal(10 + 0.5)
  f3$fun(pos = 12, time = 13, 
         data_pos = 1:11, data_time = 1:11, 
         all_time = 1:11) %>% expect_equal(10 + 0.25)
})
