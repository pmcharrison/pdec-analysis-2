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

# This analysis is slow (~ 2 days to complete).
# For a faster approximation, set downsample = 20 and fake_tone_len_ms = TRUE.
res <- model(dat)

saveRDS(res, "output/dat-analysis.rds")

beep(4)
