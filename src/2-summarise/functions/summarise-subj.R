summarise_subj <- function(dat) {
  res <- dat %>% 
    filter(cond == "randreg" & response == "hit") %>% 
    group_by(alphabet_size, tone_len_ms) %>% 
    summarise(mean = mean(rt_norm_tones_from_repeat),
              median = median(rt_norm_tones_from_repeat),
              sd = sd(rt_norm_tones_from_repeat),
              quantile_25 = quantile(rt_norm_tones_from_repeat, probs = 0.25),
              quantile_75 = quantile(rt_norm_tones_from_repeat, probs = 0.75),
              n = n() 
              # se = sd / sqrt(n), # not valid because of repetition within participants
              # ci_95_min = mean - 1.96 * se,
              # ci_95_max = mean + 1.96 * se
              ) %>% 
    ungroup()
  class(res) <- c("summary_subj", class(res))
  res
}

plot_subj <- function(dat_response) {
  dat_response %>% 
    filter(cond == "randreg" & response == "hit") %>% 
    mutate(
      alphabet_size = as.character(alphabet_size),
      tone_len_ms = as.character(tone_len_ms)
    ) %>% 
    ggplot() +
    # geom_bar(stat = "identity", position = "dodge", width = 0.9, colour = "black") +
    geom_violin(aes(x = alphabet_size, 
                    y = rt_norm_tones_from_repeat,
                    fill = tone_len_ms),
                colour = "black") +
    geom_boxplot(aes(x = alphabet_size, 
                     y = rt_norm_tones_from_repeat,
                     group = paste(alphabet_size, tone_len_ms)),
                 position = position_dodge(width = 0.9), 
                 width = 0.1,
                 fill = "white") +
    scale_x_discrete("Alphabet size") +  
    scale_y_continuous("Response time (in tones)") +
    scale_fill_manual("Tone length (ms)",
                      values = c("#E8E410", "#11A3FF", "#B50000") %>% rev) +
    theme_classic() +
    theme(aspect.ratio = 1,
          panel.grid.major = element_line(colour = "lightgrey"))
}
