summarise_models <- function(y) {
  res <- map(seq_len(nrow(y)),
      ~ cbind(y[., ] %>% select(- res),
              y$res[[.]]) %>% as_tibble) %>% 
    bind_rows() %>% 
    select(- detail) %>% 
    mutate(
      error_count = map_int(lag_tones, ~ sum(is_lag_invalid(.))),
      mean = map_dbl(lag_tones, ~ mean(exclude_invalid_lags(.))),
      sd = map_dbl(lag_tones, ~ sd(exclude_invalid_lags(.))),
      n = map_int(lag_tones, ~ length(exclude_invalid_lags(.))),
      se = sd / sqrt(n),
      ci_95_min = mean - qnorm(0.975) * se,
      ci_95_max = mean + qnorm(0.975) * se
    ) 
  # if (any(res$error_count > 0)) 
  #   warning("errors found in change-point detection")
  class(res) <- c("summary_model", class(res))
  res
}

is_lag_invalid <- function(x) {
  is.na(x) | x <= 0 | x >= 50
}

exclude_invalid_lags <- function(x) {
  x[!is_lag_invalid(x)]
}

plot_model <- function(summary_model, summary_subj, ...) {
  d1 <- summary_model %>% 
    mutate(
      alphabet_size = as.character(alphabet_size),
      tone_len_ms = as.character(tone_len_ms),
      label = factor("Model"),
      group = gsub("`", "", group),
      group = factor(group, levels = unique(group)),
      order_bound = paste("Order bound =", order_bound),
      order_bound = factor(order_bound, levels = unique(order_bound))
    )
  
  d2 <- summary_subj$cond_means %>%
    mutate(alphabet_size = factor(alphabet_size))
           # label = factor("Participants"))
  
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
                             y = median, 
                             group = tone_len_ms),
                             # shape = label),
               position = position_dodge(width = 0.9),
               fill = "white", alpha = 1, shape = 21) +
    scale_x_discrete("Cycle length (tones)") +
    scale_y_continuous("Response time (tones)") +
    scale_fill_manual("Tone length (ms)",
                      values = c("#E8E410", "#11A3FF", "#B50000") %>% rev,
                      guide = FALSE) +
    # scale_shape_manual("", values = 21) +
    # scale_linetype_discrete("", guide = guide_legend(override.aes = list(
    #   fill = "white"))) +
    theme_classic() +
    theme(aspect.ratio = 1,
          panel.grid.major = element_line(colour = "lightgrey"))
}
