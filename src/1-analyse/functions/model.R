get_change_point_spec <- function(method = "Lepage",
                                  t1_error_rate = 10000,
                                  startup = 20) {
  as.list(environment())
}

model_trial <- function(trial, ppm_spec, change_point_spec, alphabet,  ...) {
  UseMethod("model_trial")
}

model_trial.data.frame <- function(trial, ppm_spec, change_point_spec, alphabet, ...) {
  stopifnot(nrow(trial) == 1L)
  tones <- trial$stim[[1]]$tone
  model_trial(tones, ppm_spec, change_point_spec, alphabet,
              alphabet_size = trial$alphabet_size,
              tone_len_ms = trial$tone_len_ms,
              transition = trial$transition)
}

model_trial.numeric <- function(trial, 
                                ppm_spec,
                                change_point_spec,
                                alphabet,
                                alphabet_size, 
                                tone_len_ms,
                                transition,
                                ...) {
  info <- as.list(environment())
  profile <- ppm_trial(stim, alphabet_size, tone_len_ms, ppm_spec, alphabet)
  change_point <- change_point_trial(profile$information_content,
                                     transition,
                                     alphabet_size,
                                     change_point_spec)
  res <- list(info = info,
              profile = profile,
              change_point = change_point)
  class(res) <- c("trial_analysis", class(res))
  res
}

change_point_trial <- function(x, transition, alphabet_size, spec) {
  cp <- cpm::detectChangePoint(x, 
                               cpmType = spec$method, 
                               ARL0 = spec$t1_error_rate,
                               startup = spec$startup)
  cp_stat <- rep(as.numeric(NA), times = length(x))
  cp_stat[seq_along(cp$Ds)] <- cp$Ds
  cp_stat[seq_len(spec$startup - 1L)] <- as.numeric(NA)
  list(
    statistic = cp_stat,
    change_detected = cp$changeDetected,
    pos_when_change_detected = cp$detectionTime,
    lag_tones = cp$detectionTime - (transition + alphabet_size)
  )
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
# ppm_trial <- memoise(ppm_trial, cache = cache_filesystem("cache/ppm_trial"))


plot.trial_analysis <- function(x, ...) {
  p <- x$profile %>% 
    mutate(cp_stat = x$change_point$statistic) %>% 
    select(pos, information_content, cp_stat) %>% 
    gather(var, value, - pos) %>% 
    na.omit() %>% 
    mutate(var = recode(var, 
                        cp_stat = "Change-point analysis",
                        information_content = "Information content")) %>% 
    ggplot(aes(x = pos, y = value)) +
    geom_point(shape = 21, size = 3) + 
    scale_x_continuous("Tone number", 
                       sec.axis = sec_axis(~ spline(x$profile$pos,
                                                    x$profile$time,
                                                    xout = .,
                                                    method = "natural")$y,
                                           name = "Time (seconds)")) +
    scale_y_continuous("Statistic") +
    facet_wrap(~ var, ncol = 1, scales = "free_y") +
    ggtitle(glue("Lag = {x$change_point$lag_tones} tones")) +
    theme_classic()
  
  if (!is.na(x$info$transition))
    p <- p + geom_vline(xintercept = x$info$transition + x$info$alphabet_size)
  
  if (x$change_point$change_detected)
    p <- p + geom_vline(xintercept = x$change_point$pos_when_change_detected,
                         colour = "red", linetype = "dashed")
  
  p
}
