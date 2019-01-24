library(tidyverse)
library(checkmate)
library(futile.logger)
library(memoise)
library(glue)
library(withr)
library(beepr)
loadNamespace("cpm")
loadNamespace("R.utils")
loadNamespace("plyr")
theme_set(theme_classic())

for (f in list.files("src/1-analyse/functions/", full.names = TRUE))
  source(f)

dat <- readRDS("output/dat-response.rds")

model_detail <- model(dat = dat, downsample = 5)
model_summary <- summarise_models(model_detail)
subj_summary <- get_subj_summary(dat)
combined_summary <- get_combined_summary(model_summary, subj_summary)
print(plot(combined_summary))

saveRDS(list(
  detail = model_detail,
  summary = list(
    model = model_summary,
    subj = subj_summary,
    all = combined_summary
  )
), "output/analysis.rds")

if (FALSE) {
  View(model_summary)
  model_detail %>% 
    filter(grepl("Y", label)) %>% 
    {.$res[[1]]} %>%
    filter(alphabet_size == 20 & tone_len_ms == 75) %>% 
    pull(detail) %>%
    {.[[1]]} %>% 
    {.$res[[3]]} %>% plot
  # {.$profile} %>% View
  plot
}

beep(4)
