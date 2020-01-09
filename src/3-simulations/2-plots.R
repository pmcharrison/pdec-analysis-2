library(tidyverse)

source("src/3-simulations/common.R")
theme_set(ggpubr::theme_pubr() + 
            theme(strip.background = element_rect(fill = "white")))

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
ggsave("exp-1.png", path = out_dir, width = 8.65, height = 7.85, dpi = 300)


exp_1$data %>% 
  gather("key", "value", - seq_id) %>% 
  group_by(key) %>% 
  summarise(median(value))

exp_1$data %>% 
  mutate(diff = Original - `+ Decay`) %>% 
  pull(diff) %>% 
  median()


exp_2 %>% 
  set_names(plyr::revalue(names(exp_2),
                          replace = c(`Popular` = "Popular music",
                                      `Jazz` = "Jazz music"),
                          warn_missing = FALSE)) %>% 
  plot_multi_exp(trim_sd = 3) + 
  theme(panel.spacing = unit(1.5, "lines"),
        strip.text = element_text(size = 12, face = "bold"),
        strip.background = element_rect(colour = "white"))
ggsave("exp-2.png", path = out_dir, width = 6, height = 7)
ggsave("exp-2.pdf", path = out_dir, width = 6, height = 7)
