library(tidyverse)
library(checkmate)
library(futile.logger)
library(memoise)
library(glue)
library(withr)
loadNamespace("R.utils")
loadNamespace("plyr")
theme_set(theme_classic())

for (f in list.files("src/1-analyse/functions/", full.names = TRUE))
  source(f)

dat <- readRDS("output/dat-response.rds")

alphabet <- dat$stim %>% bind_rows() %>% pull(tone) %>% unique() %>% sort()

subj_rt <- get_subj_rt(dat)
ppm <- get_ppm_spec()

model <- function(dat, change_point_spec, alphabet, downsample) {
  x <- get_ppm_spec()
  x$res <- plyr::alply(x, 1, model_conditions, dat, change_point_spec, alphabet, downsample)
  x
}

model_conditions <- function(ppm_spec,
                             dat, 
                             change_point_spec,
                             alphabet, 
                             downsample) {
  flog.info(glue("Applying PPM variant '{ppm_spec$label}'..."))
  ppm_spec$label <- NULL
  cond <- get_cond(dat)
  cond %>% 
    mutate(detail = map2(alphabet_size, tone_len_ms,
                         model_condition,
                         dat, ppm_spec, change_point_spec, alphabet, downsample),
           lag_tones = map(detail, 
                           ~ map_int(.$res, ~ .$change_point$lag_tones)))
}

y <- model(dat = dat,
           change_point_spec = get_change_point_spec(),
           alphabet = alphabet,
           downsample = 2)

# p <- list()
# p$subj_rt <- plot_subj_rt(dat)
