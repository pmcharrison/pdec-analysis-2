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
  PPMdecay::ppm_options(
    decay = buffer_decay_fun(buffer_type = x$buffer_type,
                             buffer_len = x$buffer_len,
                             buffer_rate = x$buffer_rate,
                             stm_half_life = x$stm_half_life,
                             stm_rate = x$stm_rate,
                             ltm_rate = x$ltm_rate,
                             noise = x$noise),
    order_bound = x$order_bound
  )
}

buffer_decay_fun <- function(buffer_type, buffer_len, buffer_rate,
                             stm_half_life, stm_rate, ltm_rate, noise) {
  checkmate::qassert(buffer_type, "s1")
  checkmate::qassert(buffer_len, "N[0,]")
  checkmate::qassert(buffer_rate, "N[0,)")
  stopifnot(stm_half_life != 0,
            buffer_type %in% c(NA, "time", "item"),
            !(buffer_len > 0 && is.na(buffer_type)))
  PPMdecay::decay_fun(function(pos, time, data_pos, data_time, all_time, ...) {
    if (any(data_time > time)) 
      stop("tried to predict using training data from the future")
    if (any(data_pos < 1))
      stop("data_pos cannot be less than 0")
    n <- length(data_pos)
    data_buffer_fail_time = if (is.na(buffer_type)) {
      data_time
    } else if (buffer_type == "item") {
      all_time[data_pos + buffer_len]
    } else if (buffer_type == "time") {
      data_time + buffer_len 
    } else stop("invalid buffer_type argument")
    data_time_since_buffer_fail = time - data_buffer_fail_time
    weights <- if_else(is.na(data_time_since_buffer_fail) |
                         data_time_since_buffer_fail < 0,
                       buffer_rate,
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
  f <- buffer_decay_fun(buffer_type = "item",
                        buffer_len = 10, 
                        buffer_rate = 1,
                        stm_half_life = 0.000000001, 
                        stm_rate = 1, 
                        ltm_rate = 0,
                        noise = 0)
  ## Item buffers
  # Buffer = 10 - everything at full stm_rate
  f$fun(pos = 10, time = 10, 
        data_pos = 1:9, data_time = 1:9, 
        all_time = 1:9) %>% expect_equal(9)
  # No more than 10 cases can be counted
  f$fun(pos = 16, time = 16, 
        data_pos = 1:15, data_time = 1:15, 
        all_time = 1:15) %>% expect_equal(10)
  # Now set a non-zero ltm_rate
  f2 <- buffer_decay_fun(buffer_type = "item",
                         buffer_len = 10, 
                         buffer_rate = 1,
                         stm_half_life = 0.000000001, 
                         stm_rate = 1, 
                         ltm_rate = 0.1,
                         noise = 0)
  f2$fun(pos = 16, time = 16, 
         data_pos = 1:15, data_time = 1:15, 
         all_time = 1:15) %>% expect_equal(10 + 0.5)
  # Now to distinguish time from position, 
  # we need to set a non-zero half-life.
  f3 <- buffer_decay_fun(buffer_type = "item",
                         buffer_len = 10, 
                         buffer_rate = 1,
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
  
  ## Time buffers
  f4 <- buffer_decay_fun(buffer_type = "time",
                         buffer_len = 7, 
                         buffer_rate = 1,
                         stm_half_life = 1, 
                         stm_rate = 1, 
                         ltm_rate = 0,
                         noise = 0)
  f4$fun(pos = 10, time = 10,
         data_pos = 1:10, 
         data_time = seq(from = 1, by = 0.5, length.out = 10),
         all_time = seq(from = 1, by = 0.5, length.out = 10)) %>% 
    expect_equal({
      decay_exp(2, 1, 1, 0) +
        decay_exp(1.5, 1, 1, 0) +
        decay_exp(1, 1, 1, 0) +
        decay_exp(0.5, 1, 1, 0) +
        6
    })
  ## Buffer rate
  f5 <- buffer_decay_fun(buffer_type = "time",
                         buffer_len = 7, 
                         buffer_rate = 0.5,
                         stm_half_life = 1, 
                         stm_rate = 1, 
                         ltm_rate = 0,
                         noise = 0)
  f5$fun(pos = 10, time = 10,
         data_pos = 1:10,
         data_time = seq(from = 1, by = 0.5, length.out = 10),
         # data_pos = 1:4, 
         # data_time = seq(from = 1, by = 0.5, length.out = 4),
         all_time = seq(from = 1, by = 0.5, length.out = 10)) %>% 
    expect_equal({
      decay_exp(2, 1, 1, 0) +
        decay_exp(1.5, 1, 1, 0) +
        decay_exp(1, 1, 1, 0) +
        decay_exp(0.5, 1, 1, 0) +
        1 + 5 * 0.5
    })
})
