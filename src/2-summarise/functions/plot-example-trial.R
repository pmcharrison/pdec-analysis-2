source("src/1-analyse/functions/model-trial.R")

plot_example_trial <- function(x) {
  filter(x, label == "L10 + exp.decay") %>% 
    {.$res[[1]]} %>% 
    filter(alphabet_size == 10 & tone_len_ms == 50) %>% 
    {.$detail[[1]]$res[[1]]} %>% 
    plot(lag = FALSE)
}
