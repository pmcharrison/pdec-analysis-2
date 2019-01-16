for (f in list.files("src/1-analyse/functions/", full.names = TRUE))
  source(f)

library(tidyverse)
library(checkmate)
library(memoise)
library(glue)
theme_set(theme_classic())

dat <- readRDS("output/dat-response.rds")

alphabet <- dat$stim %>% bind_rows() %>% pull(tone) %>% unique() %>% sort()
cond <- get_cond(dat)
subj_rt <- get_subj_rt(dat)


ppm <- get_ppm_spec()

row <- dat[5, ]
x <- model_trial(row, 
                 ppm_spec = ppm[1, ], 
                 change_point_spec = get_change_point_spec(), 
                 alphabet = alphabet)


p <- list()
p$subj_rt <- plot_subj_rt(dat)



# ppm_dataset <- function(alphabet_size, tone_len_ms, ppm_spec, dat, alphabet, n = NULL) {
#   set.seed(1)
#   trials <- dat %>% filter(alphabet_size == !!alphabet_size &
#                              tone_len_ms == !!tone_len_ms)
#   stopifnot(nrow(trials) > 0L)
#   if (!is.null(n)) trials <- sample_n(trials, size = n, replace = FALSE)
#   trials %>% 
#     mutate(ppm = map(stim, ppm_trial, 
#                      !!alphabet_size, !!tone_len_ms, !!ppm_spec, !!alphabet))
# }
