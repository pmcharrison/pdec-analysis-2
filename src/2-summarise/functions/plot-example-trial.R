plot_example_trial <- function(x) {
  filter(x, label == "L4 + exp.decay") %>% 
    {.$res[[1]]} %>% 
    filter(alphabet_size == 10 & tone_len_ms == 50) %>% 
    {.$detail[[1]]$res[[1]]} %>% 
    plot(lag = FALSE)
}

plot.trial_analysis <- function(x, lag = TRUE, ...) {
  p <- x$profile %>% 
    mutate(cp_stat = x$change_point$statistic,
           freq = x$info$alphabet[symbol],
           log_freq = log(freq)) %>% 
    select(pos, information_content, cp_stat, log_freq) %>% 
    gather(var, value, - pos) %>% 
    na.omit() %>% 
    mutate(var = recode_factor(var, 
                               log_freq = "Frequency (logarithm)",
                               information_content = "Information content (bits)",
                               cp_stat = "Change-point statistic"
                               
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
    f <- function(x) factor(x, levels = c("Nominal transition",
                                          "Effective transition",
                                          "Detection of transition"))
    p <- p + 
      geom_vline(aes(xintercept = x$info$trial$transition,
                     linetype = "Nominal transition",
                     colour = "Nominal transition")) +
      geom_vline(aes(xintercept = x$info$trial$transition + x$info$trial$alphabet_size,
                     linetype = "Effective transition",
                     colour = "Effective transition"))
  }
  
  if (x$change_point$change_detected)
    p <- p + geom_vline(aes(xintercept = x$change_point$pos_when_change_detected,
                            colour = "Detection of transition", 
                            linetype = "Detection of transition"))
  
  p <- p + scale_linetype_manual("", values = c(`Nominal transition` = "solid",
                                                `Effective transition` = "dashed",
                                                `Detection of transition` = "dotted"),
                                 guide = guide_legend(reverse = TRUE, 
                                                      label.position = "bottom"))
  p <- p + scale_colour_manual("", values = c(`Nominal transition` = "darkred",
                                              `Effective transition` = "darkred",
                                              `Detection of transition` = "darkred"),
                               guide = guide_legend(reverse = TRUE,
                                                    label.position = "bottom"))
  p
}
