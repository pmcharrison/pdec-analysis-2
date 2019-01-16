for (f in list.files("src/1-analyse/functions/", full.names = TRUE))
  source(f)

library(tidyverse)
library(checkmate)
library(memoise)
theme_set(theme_classic())

dat <- readRDS("output/dat-response.rds")

alphabet <- dat$stim %>% bind_rows() %>% pull(tone) %>% unique() %>% sort()
cond <- get_cond(dat)
subj_rt <- get_subj_rt(dat)


ppm <- get_ppm_spec()

ppm_trial <- function(stim, alphabet_size, tone_len_ms, ppm_spec, alphabet) {
  stim <- stim %>% 
    mutate(time = seq(from = 0, by = tone_len_ms / 1000, length.out = n()))
  PPMdecay::new_model(alphabet = alphabet) %>% 
    PPMdecay::predict_seq(seq = stim$tone, 
                          time = stim$time, 
                          save = TRUE,
                          save_distribution = FALSE, 
                          options = ppm_options_from_ppm_spec(ppm_spec))
}
# ppm_trial <- memoise(ppm_trial, cache = cache_filesystem("cache/ppm_trial"))

ppm_dataset <- function(alphabet_size, tone_len_ms, ppm_spec, dat, alphabet, n = NULL) {
  set.seed(1)
  trials <- dat %>% filter(alphabet_size == !!alphabet_size &
                             tone_len_ms == !!tone_len_ms)
  stopifnot(nrow(trials) > 0L)
  if (!is.null(n)) trials <- sample_n(trials, size = n, replace = FALSE)
  trials %>% 
    mutate(ppm = map(stim, ppm_trial, 
                     !!alphabet_size, !!tone_len_ms, !!ppm_spec, !!alphabet))
}


p <- list()
p$subj_rt <- plot_subj_rt(dat)


