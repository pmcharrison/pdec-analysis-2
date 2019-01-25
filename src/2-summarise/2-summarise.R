library(magrittr)
library(tidyverse)

for (f in list.files("src/2-summarise/functions", full.names = TRUE))
  source(f)

dat_analysis <- readRDS("output/dat-analysis.rds")
dat_response <- readRDS("output/dat-response.rds")

if (FALSE) {
  dat_analysis <- dat_analysis$detail
  dat_analysis <- dat_analysis %>% mutate(
    group = gsub("`", "", group),
    group = if_else(group == "+ Exponential decay" & noise != 0,
                    "+ Retrieval noise",
                    group)
  )
}

summary_subj <- summarise_subj(dat_response)
summary_model <- summarise_models(dat_analysis)

stats_subj <- do_stats_subj(dat_response)

p1 <- plot_subj(dat_response)
p2 <- plot_model(summary_model, summary_subj)
p3 <- plot_example_trial(dat_analysis)

P1 <- cowplot::plot_grid(p1, p2, ncol = 1, rel_heights = c(1.2, 2), labels = "AUTO")
ggsave(plot = P1, filename = "output/by-cond.eps", width = 7.5, height = 7.5)
ggsave(plot = p3, filename = "output/example-trial.eps", width = 6, height = 5)

dat_analysis %>% select(- res) %>% write_csv("output/model-par.csv")
