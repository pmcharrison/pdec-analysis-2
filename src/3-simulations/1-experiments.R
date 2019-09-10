library(tidyverse)
library(ppm)
library(markovchain)
library(futile.logger)

source("src/3-simulations/common.R")

# Recency effects are useful if the source transition probabilities
# change randomly

exp_1 <- run_exp(ppm_starting_par = new_ppm_par(ltm_half_life = 15,
                                                ltm_weight = 1,
                                                stm_duration = 0),
                 ppm_which_optim = c("ltm_half_life"),
                 ppm_optim_lower = c(0.01),
                 ppm_optim_higher = c(1e90))

exp_1 %>% 
  rename(Original = mod_orig,
         `+ Decay` = mod_forget) %>% 
  ggpubr::ggpaired(cond1 = "Original",
                   cond2 = "+ Decay",
                   palette = viridis::viridis(2),
                   fill = "condition",
                   line.color = "grey",
                   xlab = "",
                   ylab = "Cross entropy",
                   ggtheme = ggpubr::theme_pubr() +
                     theme(legend.position = "none",
                           aspect.ratio = 1))


x <- 
  run_exp(ppm_starting_par = new_ppm_par(stm_weight = 1,
                                         stm_duration = 10,
                                         ltm_weight = 0.1,
                                         ltm_half_life = 1e80),
          ppm_which_optim = c("stm_duration", "ltm_weight"),
          ppm_optim_lower = c(0, 1e-5),
          ppm_optim_higher = c(1e80, 0.9999))


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

