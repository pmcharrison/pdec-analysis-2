summarise_subj <- function(dat) {
  subj_vals <- dat %>% 
    filter(cond == "randreg" & response == "hit") %>% 
    group_by(alphabet_size, tone_len_ms, subj) %>% 
    summarise(subj_val = median(rt_norm_tones_from_repeat)
              # median = median(rt_norm_tones_from_repeat),
              # sd = sd(rt_norm_tones_from_repeat),
              # quantile_25 = quantile(rt_norm_tones_from_repeat, probs = 0.25),
              # quantile_75 = quantile(rt_norm_tones_from_repeat, probs = 0.75),
              # n_subj = n())
              # se = sd / sqrt(n), # not valid because of repetition within participants
              # ci_95_min = mean - 1.96 * se,
              # ci_95_max = mean + 1.96 * se
    ) %>% 
    ungroup()
  
  cond_means <- subj_vals %>% 
    group_by(alphabet_size, tone_len_ms) %>% 
    summarise(mean = mean(subj_val),
              median = median(subj_val),
              sd = sd(subj_val),
              n = n(),
              se = sd / sqrt(n),
              ci_95_min = mean - 1.96 * se,
              ci_95_max = mean + 1.96 * se
    ) %>% 
    ungroup()
  
  res <- list(subj_vals = subj_vals,
              cond_means = cond_means,
              d_prime = analyse_d_prime(dat),
              stat = do_stats_subj(subj_vals))
  
  class(res) <- c("summary_subj", class(res))
  res
}

analyse_d_prime <- function(dat) {
  df <- dat %>% 
    group_by(subj, alphabet_size, tone_len_ms) %>% 
    summarise(d_prime = unique(d_prime)) %>% 
    ungroup()
  
  anova <- df %>% 
    mutate_at(c("subj", "alphabet_size", "tone_len_ms"), factor) %>% 
    ez::ezANOVA(dv = d_prime,
                wid = subj,
                within = .(alphabet_size, tone_len_ms),
                type = 3,
                detailed = TRUE,
                return_aov = TRUE)

  list(data = df,
       anova = anova)
}

plot_trials <- function(dat_response) {
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
    scale_x_discrete("Cycle length (tones)") +  
    scale_y_continuous("Response time (tones)") +
    scale_fill_manual("Tone length (ms)",
                      values = viridis::viridis(3)) +
    theme_classic() +
    theme(aspect.ratio = 1,
          panel.grid.major = element_line(colour = "lightgrey"))
}
