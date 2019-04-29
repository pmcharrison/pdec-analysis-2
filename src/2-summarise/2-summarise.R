library(magrittr)
library(tidyverse)

for (f in list.files("src/2-summarise/functions", full.names = TRUE))
  source(f)

dat_analysis <- readRDS("output/dat-analysis.rds")
dat_response <- readRDS("output/dat-response.rds")

summary_subj <- summarise_subj(dat_response) # update this
summary_model <- summarise_models(dat_analysis)

p_trials <- plot_trials(dat_response)
p_subj <- plot_subj(summary_subj)
p_model <- plot_model(summary_model, summary_subj)

p_example_trial <- plot_example_trial(dat_analysis)

p_combined <- cowplot::plot_grid(p_subj, p_model, ncol = 1, rel_heights = c(1.2, 2), labels = "AUTO")
ggsave(plot = p_combined, filename = "output/by-cond.eps", width = 7.5, height = 7.5)
ggsave(plot = p_example_trial, filename = "output/example-trial.eps", width = 5.5, height = 5.5)

summary_subj %>% write_csv("output/summary-subj.csv")
summary_model %>% select(- lag_tones ) %>% write_csv("output/summary-model.csv")
dat_analysis %>% select(- res) %>% write_csv("output/model-par.csv")
