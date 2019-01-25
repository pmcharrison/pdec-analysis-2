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

plot.trial_analysis <- function(x, lag = TRUE, ...) {
  p <- x$profile %>% 
    mutate(cp_stat = x$change_point$statistic,
           symbol = as.numeric(symbol)) %>% 
    select(pos, information_content, cp_stat, symbol) %>% 
    gather(var, value, - pos) %>% 
    na.omit() %>% 
    mutate(var = recode_factor(var, 
                               symbol = "Frequency (Hz)",
                               cp_stat = "Change-point statistic",
                               information_content = "Information content (bits)"
    )) %>% 
    ggplot(aes(x = pos, y = value)) +
    geom_point(size = 1, colour = "navy") + 
    scale_x_continuous("Tone number", 
                       sec.axis = sec_axis(~ spline(x$profile$pos,
                                                    x$profile$time,
                                                    xout = .,
                                                    method = "natural")$y,
                                           name = "Time (seconds)")) +
    scale_y_continuous("Value") +
    facet_wrap(~ var, ncol = 1, scales = "free_y") +
    # theme_bw() +
    theme_classic() +
    theme(panel.grid = element_blank(), 
          strip.background = element_rect(colour = "white"),
          strip.text = element_text(hjust = 0),
          legend.key.size = unit(1, 'cm'),
          legend.key.width = unit(3.0, "cm"),
          legend.spacing.x = unit(1.0, 'cm'),
          legend.position = "bottom")
  
  if (lag) p <- p + ggtitle(glue("Lag = {x$change_point$lag_tones} tones")) 
  
  if (!is.na(x$info$trial$transition)) {
    f <- function(x) factor(x, levels = c("Phase change",
                                          "First repetition",
                                          "Detection of transition"))
    p <- p + 
      geom_vline(aes(xintercept = x$info$trial$transition,
                     linetype = "Phase change",
                     colour = "Phase change")) +
      geom_vline(aes(xintercept = x$info$trial$transition + x$info$trial$alphabet_size,
                     linetype = "First repetition",
                     colour = "First repetition"))
  }
  
  if (x$change_point$change_detected)
    p <- p + geom_vline(aes(xintercept = x$change_point$pos_when_change_detected,
                            colour = "Detection of transition", 
                            linetype = "Detection of transition"))
  
  p <- p + scale_linetype_manual("", values = c(`Phase change` = "solid",
                                                `First repetition` = "dashed",
                                                `Detection of transition` = "dotted"),
                                 guide = guide_legend(reverse = TRUE, 
                                                      label.position = "bottom"))
  p <- p + scale_colour_manual("", values = c(`Phase change` = "darkred",
                                              `First repetition` = "darkred",
                                              `Detection of transition` = "darkred"),
                               guide = guide_legend(reverse = TRUE,
                                                    label.position = "bottom"))
  
  p
}
