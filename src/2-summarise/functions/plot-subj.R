plot_subj <- function(summary_subj) {
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
    # geom_point(position = position_jitterdodge(jitter.width = 0.05, 
    #                                            dodge.width = 0.9, 
    #                                            seed = 1),
    #            shape = 21, fill = "white") +

    # scale_alpha_discrete(range = c(1, 1)) +
    scale_x_discrete("Alphabet size") +  
    scale_y_continuous("Response time (in tones)") +
    scale_fill_manual("Tone length (ms)",
                      values = c("#E8E410", "#11A3FF", "#B50000") %>% rev) +
    scale_colour_manual("Tone length (ms)",
                        values = c("black", "black", "black")) +
    theme_classic() +
    theme(aspect.ratio = 1,
          panel.grid.major = element_line(colour = "lightgrey"))
}
