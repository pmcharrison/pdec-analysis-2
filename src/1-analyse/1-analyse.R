for (f in list.files("src/1-analyse/functions/", full.names = TRUE))
  source(f)

library(tidyverse)
library(checkmate)
library(memoise)
library(glue)
library(withr)
loadNamespace("plyr")
theme_set(theme_classic())

dat <- readRDS("output/dat-response.rds")

alphabet <- dat$stim %>% bind_rows() %>% pull(tone) %>% unique() %>% sort()
cond <- get_cond(dat)
subj_rt <- get_subj_rt(dat)
ppm <- get_ppm_spec()

y <- model_condition(alphabet_size = 10, 
                     tone_len_ms = 25,
                     dat = dat, 
                     ppm_spec = ppm[1, ],
                     change_point_spec = get_change_point_spec(),
                     alphabet = alphabet,
                     downsample = 2)

plot(x)

p <- list()
p$subj_rt <- plot_subj_rt(dat)


