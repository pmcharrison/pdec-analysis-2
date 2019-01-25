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
    mutate(time = seq(from = 0, by = tone_len_ms / 1000, length.out = n()))
  PPMdecay::new_model(alphabet = alphabet) %>% 
    PPMdecay::predict_seq(seq = stim$tone, 
                          time = stim$time, 
                          save = TRUE,
                          save_distribution = FALSE, 
                          options = ppm_options_from_ppm_spec(ppm_spec))
}
