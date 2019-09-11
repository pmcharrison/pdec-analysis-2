library(tidyverse)
library(ppm)
library(markovchain)
library(futile.logger)

source("src/3-simulations/common.R")
theme_set(ggpubr::theme_pubr())

out_dir <- "output/simulations"
R.utils::mkdirs(out_dir)

# Recency effects are useful if the source transition probabilities
# change randomly

set.seed(1)
flog.info("Conducting experiment 1 simulations...")
exp_1 <- 
  new_ppm_optim(starting_par = new_ppm_par(ltm_half_life = 15,
                                           ltm_weight = 1,
                                           stm_duration = 0),
                which_optim = c("ltm_half_life"),
                optim_lower = c(0.01),
                optim_higher = c(1e90)) %>% 
  list(`+ Decay` = .) %>% 
  run_exp(alphabet_size = 5, 
          forget = TRUE, 
          ftol_rel = 1e-5, 
          metric = "incorrect")
plot_exp(exp_1)
saveRDS(exp_1, file.path(out_dir, "exp-1.rds"))

# Corpus modelling - long-term learning is useful if there is 
# shared syntax underlying the sequence in addition to local 
# variation

exp_2_ppm_optim <- list(
  "+ Decay" = new_ppm_optim(
    starting_par = new_ppm_par(stm_weight = 1, 
                               stm_duration = 0,
                               ltm_weight = 1, 
                               ltm_half_life = 50,
                               ltm_asymptote = 0,
                               order_bound = 4),
    which_optim = c("ltm_half_life"),
    optim_lower = c(0.0001),
    optim_higher = c(1e60)
  ),
  "+ Long-term learning" = new_ppm_optim(
    starting_par = new_ppm_par(stm_weight = 1, 
                               stm_duration = 0,
                               ltm_weight = 1, 
                               ltm_half_life = 15,
                               ltm_asymptote = 0.5,
                               order_bound = 4),
    which_optim = c("ltm_half_life", "ltm_asymptote"),
    optim_lower = c(0.0001, 0),
    optim_higher = c(1e60, 1)
  )
)

exp_2_corpora <- list(
  Popular = get_harmony_corpus(hcorp::popular_1, n = 100),
  Jazz = get_harmony_corpus(hcorp::jazz_1, n = 100),
  `Bach chorales` = get_harmony_corpus(hcorp::classical_1[1:370], n = 100)
) 

flog.info("Conducting experiment 2 simulations...")
exp_2_res <- 
  map2(exp_2_corpora, names(exp_2_corpora), 
       ~ with_log(paste("Analysing", .y, "corpus..."), {
         run_exp(
           alphabet_size = length(.x$alphabet), 
           corpus_generator = function() .x$corpus, 
           ppm_optim = exp_2_ppm_optim,
           forget = FALSE,
           metric = "information_content",
           progress = TRUE,
           ftol_rel = 1e-3
         )
       }))

plot_multi_exp(exp_2_res, trim_sd = 3)
saveRDS(exp_2_res, file.path(out_dir, "exp-2.rds"))
saveRDS(exp_2_corpora, file.path(out_dir, "exp-2-corpora.rds"))

# t.test(exp_2b$data$`+ Decay`,
#        exp_2b$data$`+ Long-term learning`,
#        paired = TRUE)
