library(tidyverse)

source("src/3-simulations/common.R")
theme_set(ggpubr::theme_pubr())

out_dir <- "output/simulations"

exp_1 <- readRDS(file.path(out_dir, "exp-1.rds"))
exp_2 <- readRDS(file.path(out_dir, "exp-2.rds"))

cowplot::plot_grid(
  plot_markov_model_series(),
  cowplot::plot_grid(
    plot_exp_old(exp_1),
    plot_exp(exp_1),
    nrow = 1,
    labels = c("B", "C")
  ),
  labels = c("A", ""),
  ncol = 1,
  rel_heights = c(2, 1)
)
ggsave("exp-1.pdf", path = out_dir, width = 8.65, height = 7.85)


exp_2 %>% 
  set_names(plyr::revalue(names(exp_2),
                          replace = c(`Popular` = "Popular music",
                                      `Jazz` = "Jazz music"),
                          warn_missing = FALSE)) %>% 
  plot_multi_exp(trim_sd = 3)
ggsave("exp-2.pdf", path = out_dir, width = 6, height = 7)
