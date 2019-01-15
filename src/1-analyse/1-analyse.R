for (f in list.files("src/1-analyse/functions/", full.names = TRUE))
  source(f)

library(tidyverse)
theme_set(theme_classic())

dat <- readRDS("output/dat-response.rds")

p <- list()
p$subj_rt <- plot_subj_rt(dat)
