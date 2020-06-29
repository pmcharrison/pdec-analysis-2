checkpoint::checkpoint("2019-12-01")

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
theme_set(ggpubr::theme_pubr())

for (f in list.files("src/1-analyse/functions/", full.names = TRUE))
  source(f)

dat <- readRDS("output/dat-response.rds")

res <- model(dat)

saveRDS(res, "output/dat-analysis.rds")

beep(4)
