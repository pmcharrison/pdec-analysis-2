plot_subj <- function(summary_subj) {
  p_d_prime <- plot_d_prime(summary_subj, xlab = FALSE)
  p_rt <- plot_subj_rt(summary_subj, xlab = FALSE)
  p_rt_diff <- plot_subj_rt_diff(summary_subj)
  
  cowplot::plot_grid(p_d_prime,
                     p_rt,
                     p_rt_diff,
                     ncol = 1,
                     labels = "AUTO")
}

plot_subj_rt_diff <- function(summary_subj) {
  summary_subj$stat$condition_differences$pairwise %>% 
    mutate(
      alphabet_size = as.character(alphabet_size),
      tone_len_ms = as.character(tone_len_ms)
    ) %>% 
    ggplot(aes(x = alphabet_size, 
               y = diff,
               fill = tone_len_ms,
               colour = tone_len_ms)) +
    geom_hline(yintercept = 0) +
    geom_violin(colour = "black") +
    geom_boxplot(aes(x = alphabet_size,
                     y = diff,
                     group = paste(alphabet_size, tone_len_ms)),
                 position = position_dodge(width = 0.9),
                 width = 0.1,
                 fill = "white",
                 outlier.shape = 21,
                 show.legend = FALSE) +
    scale_x_discrete("Cycle length (tones)") +  
    scale_y_continuous("Relative response time (tones)") +
    scale_fill_manual("Tone length (ms)",
                      values = viridis::viridis(3)[2:3]) +
    scale_colour_manual("Tone length (ms)",
                        values = c("black", "black", "black")) +
    ggpubr::theme_pubr() +
    theme(aspect.ratio = 1,
          legend.position = "right")
          # panel.grid.major = element_line(colour = "lightgrey"))
}

plot_d_prime <- function(summary_subj, xlab) {
  summary_subj$d_prime$data %>% 
    mutate(
      alphabet_size = as.character(alphabet_size),
      tone_len_ms = as.character(tone_len_ms)
    ) %>% 
    ggplot(aes(x = alphabet_size, 
               y = d_prime,
               fill = tone_len_ms,
               colour = tone_len_ms)) +
    geom_violin(colour = "black") +
    geom_boxplot(aes(x = alphabet_size,
                     y = d_prime,
                     group = paste(alphabet_size, tone_len_ms)),
                 position = position_dodge(width = 0.9),
                 width = 0.1,
                 fill = "white",
                 outlier.shape = 21,
                 show.legend = FALSE) +
    scale_x_discrete(if (xlab) "Cycle length (tones)" else "") +  
    scale_y_continuous("Sensitivity (d')") +
    scale_fill_manual("Tone length (ms)",
                      values = viridis::viridis(3)) +
    scale_colour_manual("Tone length (ms)",
                        values = c("black", "black", "black")) +
    ggpubr::theme_pubr() +
    theme(aspect.ratio = 1,
          legend.position = "right")
          # panel.grid.major = element_line(colour = "lightgrey"))
}


plot_subj_rt <- function(summary_subj, xlab = TRUE) {
  summary_subj$subj_vals %>%
    mutate(
      alphabet_size = as.character(alphabet_size),
      tone_len_ms = as.character(tone_len_ms)
    ) %>% 
    ggplot(aes(x = alphabet_size, 
               y = subj_val,
               fill = tone_len_ms,
               colour = tone_len_ms)) +
    geom_violin(colour = "black") +
    geom_boxplot(aes(x = alphabet_size,
                     y = subj_val,
                     group = paste(alphabet_size, tone_len_ms)),
                 position = position_dodge(width = 0.9),
                 width = 0.1,
                 fill = "white",
                 outlier.shape = 21,
                 show.legend = FALSE) +
    scale_x_discrete(if (xlab) "Cycle length (tones)" else "") +  
    scale_y_continuous("Response time (tones)") +
    scale_fill_manual("Tone length (ms)",
                      values = viridis::viridis(3)) +
                      # values = c("#E8E410", "#11A3FF", "#B50000") %>% rev) +
    scale_colour_manual("Tone length (ms)",
                        values = c("black", "black", "black")) +
    ggpubr::theme_pubr() +
    theme(aspect.ratio = 1,
          legend.position = "right")
          # panel.grid.major = element_line(colour = "lightgrey"))
}
