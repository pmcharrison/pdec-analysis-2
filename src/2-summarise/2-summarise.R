library(checkpoint)
checkpoint("2019-12-01")

library(magrittr)
library(tidyverse)

for (f in list.files("src/2-summarise/functions", full.names = TRUE))
  source(f)

dat_analysis <- readRDS("output/dat-analysis.rds")
dat_sensitivity <- readRDS("output/sensitivity-datasets.rds")
dat_response <- readRDS("output/dat-response.rds")

model_misses <- tabulate_model_misses(dat_analysis)
if (any(model_misses$max_lag >= 50))
  stop("Detected model lags greater than or equal to 50")
write_csv(model_misses, "output/model-misses.csv")

summary_subj <- summarise_subj(dat_response)
summary_model <- summarise_models(dat_analysis)
summary_sensitivity <- summarise_models(dat_sensitivity)

model_fits <- get_model_fits(summary_model, summary_subj)
sensitivity_fits <- get_model_fits(summary_sensitivity, summary_subj)

# Compare ICC for optimised dataset versus perturbed dataset
model_fits$icc[6]
mean(sensitivity_fits$icc)
# Same for Pearson correlation
model_fits$cor_pearson[6]
mean(sensitivity_fits$cor_pearson)

p_trials <- plot_trials(dat_response)
p_subj <- plot_subj(summary_subj)
p_model <- plot_model(summary_model, summary_subj)

p_example_trial <- plot_example_trial(dat_analysis)

# p_combined <- cowplot::plot_grid(p_subj, p_model[[1]], p_model[[2]],
#                                  ncol = 1,
#                                  rel_heights = c(1.6, 1, 1),
#                                  labels = "AUTO")
# ggsave(plot = p_combined, filename = "output/by-cond.pdf", width = 6.5, height = 8)

ggsave(plot = p_subj, filename = "output/behavioral.pdf", width = 4.75, height = 8, dpi = 300)
ggsave(plot = p_model, filename = "output/model-results.pdf", width = 6, height = 6, dpi = 300)

ggsave(plot = p_example_trial, filename = "output/example-trial.pdf", width = 5.5, height = 5.5, dpi = 300)

summary_subj %>% saveRDS("output/summary-subj.rds")
summary_model %>% select(- lag_tones ) %>% write_csv("output/summary-model.csv")
dat_analysis %>% select(- res) %>% write_csv("output/model-par.csv")
