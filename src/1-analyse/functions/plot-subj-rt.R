plot_subj_rt <- function(dat) {
  dat %>% 
    filter(cond == "randreg" & response == "hit") %>% 
    group_by(alphabet_size, tone_len_ms) %>% 
    summarise(mean = mean(rt_norm_tones_from_repeat),
              sd = sd(rt_norm_tones_from_repeat),
              n = n(), 
              se = sd / sqrt(n),
              ymin = mean - 1.96 * se,
              ymax = mean + 1.96 * se) %>% 
    ungroup() %>% 
    mutate(
      alphabet_size = as.character(alphabet_size),
      tone_len_ms = as.character(tone_len_ms)
    ) %>% 
    ggplot(aes(x = alphabet_size, 
               y = mean, 
               ymin = ymin,
               ymax = ymax,
               fill = tone_len_ms)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.9, colour = "black") +
    geom_errorbar(position = position_dodge(width = 0.9), width = 0.2) +
    scale_x_discrete("Alphabet size") +  
    scale_y_continuous("Response time (in tones)") +
    scale_fill_discrete("Tone length (ms)") +
    theme(aspect.ratio = 1)
}
