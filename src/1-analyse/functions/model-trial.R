library(memoise)
library(glue)

get_change_point_spec <- function(method = "Mann-Whitney",
                                  t1_error_rate = 10000,
                                  startup = 20) {
  as.list(environment())
}

model_trial <- function(trial, 
                        ppm_spec,
                        change_point_spec,
                        alphabet,
                        ...) {
  stopifnot(
    is.data.frame(trial),
    nrow(trial) == 1L
  )
  info <- as.list(environment())
  profile <- ppm_trial(trial$stim[[1]], 
                       alphabet_size = trial$alphabet_size, 
                       tone_len_ms = trial$tone_len_ms, 
                       ppm_spec = ppm_spec, 
                       alphabet = alphabet)
  change_point <- change_point_trial(profile$information_content,
                                     transition = trial$transition,
                                     alphabet_size = trial$alphabet_size,
                                     spec = change_point_spec)
  res <- list(info = info,
              profile = profile,
              change_point = change_point)
  class(res) <- c("trial_analysis", class(res))
  res
}

R.utils::mkdirs("cache/model_trial")
model_trial <- memoise(model_trial, cache = cache_filesystem("cache/model_trial"))

change_point_trial <- function(x, transition, alphabet_size, spec) {
  cp <- cpm::detectChangePoint(x, 
                               cpmType = spec$method, 
                               ARL0 = spec$t1_error_rate,
                               startup = spec$startup)
  cp_stat <- rep(as.numeric(NA), times = length(x))
  cp_stat[seq_along(cp$Ds)] <- cp$Ds
  cp_stat[seq_len(spec$startup - 1L)] <- as.numeric(NA)
  res <- list(
    statistic = cp_stat,
    change_detected = cp$changeDetected,
    pos_when_change_detected = if (cp$changeDetected) cp$detectionTime else as.integer(NA)
  )
  res$lag_tones <- res$pos_when_change_detected - (transition + alphabet_size)
  res
}

ppm_trial <- function(stim, alphabet_size, tone_len_ms, ppm_spec, alphabet) {
  stim <- stim %>% 
    mutate(time = seq(from = 0, by = tone_len_ms / 1000, length.out = n()),
           symbol = factor(tone, levels = alphabet))
  ppm::new_ppm_decay(
    alphabet_size = length(alphabet),
    order_bound = ppm_spec$order_bound,
    buffer_length_time = ppm_spec$buffer_time,
    buffer_length_items = ppm_spec$buffer_items,
    buffer_weight = ppm_spec$buffer_rate, 
    only_learn_from_buffer = ppm_spec$only_learn_from_buffer,
    only_predict_from_buffer = ppm_spec$only_predict_from_buffer,
    stm_half_life = ppm_spec$stm_half_life, 
    stm_weight = ppm_spec$stm_rate,
    ltm_weight = ppm_spec$ltm_rate, 
    noise = ppm_spec$noise
  ) %>% 
    ppm::model_seq(seq = stim$symbol, 
                   time = stim$time, 
                   return_distribution = FALSE)
}
