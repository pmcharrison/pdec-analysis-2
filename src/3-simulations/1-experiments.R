library(tidyverse)
library(ppm)
library(markovchain)
library(futile.logger)

source("src/3-simulations/common.R")
theme_set(ggpubr::theme_pubr())

# Recency effects are useful if the source transition probabilities
# change randomly

par_decay <- new_ppm_optim(starting_par = new_ppm_par(ltm_half_life = 15,
                                                      ltm_weight = 1,
                                                      stm_duration = 0),
                           which_optim = c("ltm_half_life"),
                           optim_lower = c(0.01),
                           optim_higher = c(1e90))

exp_1 <- run_exp(ppm_optim = list(`+ Decay` = par_decay))

plot_exp(exp_1)


## Corpus modelling

par_music_stm <- new_ppm_optim(
  starting_par = new_ppm_par(stm_weight = 1, 
                             stm_duration = 500,
                             ltm_weight = 0.5, 
                             ltm_half_life = 1e-50,
                             ltm_asymptote = 0,
                             order_bound = 4),
  which_optim = c("ltm_weight"),
  optim_lower = c(0.0001),
  optim_higher = c(1)
)

par_music_ltm <- new_ppm_optim(
  starting_par = new_ppm_par(stm_weight = 1, 
                             stm_duration = 500,
                             ltm_weight = 0.5, 
                             ltm_half_life = 1e-50,
                             ltm_asymptote = 0,
                             order_bound = 4),
  which_optim = c("ltm_weight", "ltm_asymptote"),
  optim_lower = c(0.0001, 0),
  optim_higher = c(1, 1)
)

popular_1 <- get_harmony_corpus("popular_1")

exp_2 <- run_exp(alphabet_size = length(popular_1$alphabet), 
                 corpus_generator = function() popular_1$corpus[1:100], 
                 ppm_optim = list("+ Decay" = par_music_stm,
                                  "+ Long-term learning" = par_music_ltm),
                 forget = FALSE,
                 metric = "information_content",
                 progress = TRUE)
plot_exp(exp_2, alpha = 0.25, linetype = "solid")
plot_exp_2(exp_2)


jazz_1 <- get_harmony_corpus("jazz_1")
exp_2b <- run_exp(alphabet_size = length(jazz_1$alphabet), 
                  corpus_generator = function() jazz_1$corpus[1:100], 
                  ppm_optim = list("+ Decay" = par_music_stm,
                                   "+ Long-term learning" = par_music_ltm),
                  forget = FALSE,
                  metric = "information_content",
                  progress = TRUE)
plot_exp_2(exp_2b)

t.test(exp_2b$data$`+ Decay`,
       exp_2b$data$`+ Long-term learning`,
       paired = TRUE)


# par_2 <- exp_2$par$`+ Long-term learning`
# par_2[["ltm_asymptote"]] <- 0
# 
# eval_ppm_mod(ppm_par = par_2, 
#              corpus = popular_1$corpus[1:100], 
#              forget = FALSE,
#              alphabet_size = popular_1$alphabet %>% length(), 
#              metric = "information_content", 
#              progress = TRUE) %>% mean()
# 
# exp_2$data %>% 
#   mutate(diff = .data$`+ Long-term learning` - .data$`+ Decay`) %>% 
#   ggplot(aes(x = diff)) + 
#   geom_histogram()
# 
# t.test(exp_2$data$`+ Decay`,
#        exp_2$data$`+ Long-term learning`,
#        paired = TRUE)

## Now using an informative prior:

# par_ltm <- new_ppm_optim(starting_par = new_ppm_par(stm_weight = 1,
#                                                     ltm_weight = 0.1,
#                                                     stm_duration = 15,
#                                                     ltm_half_life = 1e80),
#                          which_optim = c("ltm_weight", "stm_duration"),
#                          optim_lower = c(1e-6, 0),
#                          optim_higher = c(0.9999, 1e50))
# 
# exp_2 <- run_exp(
#   alphabet_size = 5,
#   ppm_optim = list(`+ Decay` = par_decay,
#                    `+ Long-term memory` = par_ltm),
#   corpus_opt = new_corpus_opt(alphabet_size = 5, 
#                               informative_prior = TRUE,
#                               alpha_1 = rep(1, times = 5),
#                               alpha_2 = rep(1, times = 5))
# )

plot_exp(exp_1)
plot_exp(exp_2)
# x <- 
#   run_exp(ppm_starting_par = new_ppm_par(stm_weight = 1,
#                                          stm_duration = 10,
#                                          ltm_weight = 0.1,
#                                          ltm_half_life = 1e80),
#           ppm_which_optim = c("stm_duration", "ltm_weight"),
#           ppm_optim_lower = c(0, 1e-5),
#           ppm_optim_higher = c(1e80, 0.9999))


# new_ppm_simple(alphabet_size, order_bound = 1) %>% 
#   model_seq(df$symbol, return_distribution = FALSE, return_entropy = FALSE) %>% 
#   mutate(id = seq_along(symbol)) %>% 
#   ggplot(aes(id, information_content)) + 
#   # scale_x_continuous(limits = c(NA, 100)) +
#   geom_line() + 
#   geom_smooth() +
#   geom_vline(aes(xintercept = pos),
#              colour = "red",
#              data = df %>% filter(change) %>% mutate(pos = id - 0.5))

