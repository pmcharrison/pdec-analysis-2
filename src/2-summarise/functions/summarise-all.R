summarise_all <- function(model_summary, subj_summary) {
  x <- model_summary %>% 
    select(label, alphabet_size, tone_len_ms,
           mean, sd, n, se, ci_95_min, ci_95_max)
  y <- subj_summary %>% 
    add_column(label = "Participants", .before = 1)
  res <- bind_rows(x, y)
  class(res) <- c("summarise_all", class(res))
  res
}

# plot.combined_summary <- function(x, ...) {
#   x %>% mutate(
#     alphabet_size = as.character(alphabet_size),
#     tone_len_ms = as.character(tone_len_ms),
#     label = factor(label, levels = c("Participants", get_ppm_spec()$label))
#   ) %>% 
#     ggplot(aes(x = alphabet_size, 
#                y = mean, 
#                ymin = ci_95_min,
#                ymax = ci_95_max,
#                fill = tone_len_ms)) +
#     geom_bar(stat = "identity", position = "dodge", width = 0.9, colour = "black") +
#     geom_errorbar(position = position_dodge(width = 0.9), width = 0.2) +
#     scale_x_discrete("Alphabet size") +  
#     scale_y_continuous("Reaction time (tones)") +
#     scale_fill_discrete("Tone length (ms)") +
#     facet_wrap(~ label) +
#     theme(aspect.ratio = 1)
# }
