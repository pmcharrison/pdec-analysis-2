library(tidyverse)

for (f in list.files("src/2-plot/functions", full.names = TRUE))
  source(f)
source("src/1-analyse/functions/model-trial.R")


for (f in list.files(""))

x <- readRDS("output/analysis.rds")
y <- readRDS("output/dat-response.rds")

p1 <- plot_subj_rt(y)
p2 <- plot_models(x)

x$detail %>% 
  filter(label == "L10 + exp.decay") %>% 
  {.$res[[1]]} %>% 
  filter(alphabet_size == 10 & tone_len_ms == 50) %>% 
  {.$detail[[1]]$res[[1]]} %>% 
  plot(lag = FALSE)

p3 <- plot(x$detail$res$`1`$detail[[1]]$res[[1]])

cowplot::plot_grid(p1, p2, ncol = 1, rel_heights = c(1.2, 2),
                   labels = "AUTO")


