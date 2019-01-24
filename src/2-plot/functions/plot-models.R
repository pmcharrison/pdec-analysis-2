plot_models <- function(x) {
  d1 <- x$summary$model %>% 
    mutate(
      alphabet_size = as.character(alphabet_size),
      tone_len_ms = as.character(tone_len_ms),
      label = factor("Model"),
      group = gsub("`", "", group),
      group = factor(group, levels = unique(group)),
      order_bound = paste("Order bound =", order_bound),
      order_bound = factor(order_bound, levels = unique(order_bound))
    )
  
  d2 <- x$summary$subj %>%
    mutate(alphabet_size = factor(alphabet_size),
           label = factor("Participants"))
  
  ggplot(data = d1) +
    geom_bar(data = d1,
             aes(x = alphabet_size, 
                 y = mean, 
                 fill = tone_len_ms),
             stat = "identity", position = "dodge", width = 0.9, colour = "black") +
    facet_grid(order_bound ~ group) +
    geom_errorbar(data = d1,
                  aes(x = alphabet_size,
                      ymin = ci_95_min,
                      ymax = ci_95_max,
                      group = tone_len_ms), 
                  position = position_dodge(width = 0.9), 
                  width = 0.2) +
    geom_point(data = d2, 
               mapping = aes(x = alphabet_size, 
                             y = mean, 
                             group = tone_len_ms,
                             shape = label),
               position = position_dodge(width = 0.9),
               fill = "white", alpha = 1) +
    scale_x_discrete("Alphabet size") +
    scale_y_continuous("Response time (in tones)") +
    scale_fill_manual("Tone length (ms)",
                      values = c("#E8E410", "#11A3FF", "#B50000") %>% rev,
                      guide = FALSE) +
    scale_shape_manual("", values = 21) +
    # scale_linetype_discrete("", guide = guide_legend(override.aes = list(
    #   fill = "white"))) +
    theme_classic() +
    theme(aspect.ratio = 1,
          panel.grid.major = element_line(colour = "lightgrey"))
}
