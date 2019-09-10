library(tidyverse)
library(ppm)
library(extraDistr)
library(markovchain)
library(futile.logger)

norm_dist <- function(x) x / sum(x)

#' Generate Markov matrix
#' 
#' Each row of the transition matrix is created by averaging together 
#' the 0th-order distribution with a randomly sampled distribution 
#' from a flat Dirichlet prior.
#' 
#' @param alpha_1 Prior distribution for the 0th-order distribution
#' @param alpha_2 Prior distribution for the 1st-order conditional distributions
#' 
#' @return An object of class \code{markovchain}.
#' 
#' @seealso 
#' - \code{\link[markovchain]{rmarkovchain}} for sampling from the Markov chain
#' - \code{\link[markovchain]{steadyStates}} for computing steady states
#' 
#' @md
generate_markov_model <- function(alphabet_size, 
                                  concentration_1 = 1, 
                                  concentration_2 = 1) {
  
  checkmate::qassert(alphabet_size, "X1[2,)")
  checkmate::qassert(concentration_1, "N1(0,)")
  checkmate::qassert(concentration_2, "N1(0,)")
  alpha_1 <- rep(concentration_1, times = alphabet_size)
  alpha_2 <- rep(concentration_2, times = alphabet_size)
  
  order_1 <- as.numeric(rdirichlet(1, alpha_1))
  order_2 <-
    map(seq_len(alphabet_size), ~ 0.5 * (order_1 + as.numeric(rdirichlet(1, alpha_2)))) %>% 
    map(norm_dist) %>% 
    do.call(rbind, .)
  
  new("markovchain",
      states = as.character(seq_len(alphabet_size)),
      transitionMatrix = order_2)
}

get_unigram_probs <- function(mod) {
  steadyStates(mod) %>% as.numeric()
}

get_bigram_probs <- function(mod, unigram_probs, alphabet_size) {
  mat <- mod@transitionMatrix
  expand.grid(elt_1 = seq_len(alphabet_size),
              elt_2 = seq_len(alphabet_size)) %>% 
    as_tibble() %>% 
    mutate(probability = map2_dbl(elt_1, elt_2, ~ mat[.x, .y])) %>% 
    left_join(tibble(elt_2 = seq_len(alphabet_size),
                     unigram_probability = unigram_probs),
              by = "elt_2") %>% 
    dplyr::mutate(relative_probability = .data$probability - .data$unigram_probability)
}

plot_markov_model <- function(mod,
                              heights = c(0.25, 0.75)) {
  alphabet_size <- length(mod@states)
  unigram_probs <- get_unigram_probs(mod)
  bigram_probs <- get_bigram_probs(mod, unigram_probs, alphabet_size)
  
  breaks <- seq(
    from = min(bigram_probs$probability) %>% multiply_by(10) %>% floor() %>% divide_by(10),
    to = max(bigram_probs$probability) %>% multiply_by(10) %>% ceiling() %>% divide_by(10),
    by = 0.1
  )
  
  bigram_fill_scale <- 
    ggplot2::scale_fill_viridis_c("Probability", 
                                  breaks = breaks, limits = c(min(breaks), 
                                                              max(breaks)))
  
  egg::ggarrange(tibble(elt_1 = seq_len(alphabet_size),
                        probability = unigram_probs) %>% ppm:::plot_unigrams(),
                 ppm:::plot_bigrams(bigram_probs, bigram_fill_scale, relative = FALSE),
                 ncol = 1,
                 heights = heights)
}

plot_bigrams <- function(x, fill_scale) {
  alphabet <- sort(unique(x$elt_1))
  ggplot2::ggplot(x, ggplot2::aes_string(
    x = "elt_1", y = "elt_2", fill = "relative_probability")) +
    ggplot2::geom_tile(colour = "black", size = 0.5) +
    ggplot2::scale_x_continuous(breaks = alphabet,
                                minor_breaks = NULL,
                                name = "Continuation") +
    ggplot2::scale_y_continuous(breaks = alphabet,
                                minor_breaks = NULL,
                                name = "Context") +
    fill_scale +
    ggplot2::theme(legend.position = "bottom",
                   legend.justification = "centre") +
    ggplot2::guides(fill = ggplot2::guide_colourbar(title.position = "top", 
                                                    # hjust = 0.5, # centres the title horizontally
                                                    title.hjust = 0,
                                                    label.position = "bottom",
                                                    ticks.colour = "black",
                                                    ticks.linewidth = 1,
                                                    frame.colour = "black",
                                                    frame.linewidth = 1)) 
}

plot_unigrams <- function(x) {
  alphabet <- sort(unique(x$elt_1))
  ggplot2::ggplot(x, ggplot2::aes_string(x = "elt_1", y = "probability")) +
    ggplot2::geom_bar(stat = "identity", colour = "black", fill = "#289b87") +
    ggplot2::scale_x_continuous(breaks = alphabet, minor_breaks = NULL, name = NULL) +
    ggplot2::scale_y_continuous("Probability")
}


generate_seq <- function(alphabet_size, num_events, change_prob) {
  df <- 
    tibble(id = seq_len(num_events),
           change = as.logical(rbern(num_events, prob = change_prob)),
           segment = cumsum(change) - change[1] + 1,
           symbol = as.integer(NA))
  
  for (segment in unique(df$segment)) {
    mod <- generate_markov_model(alphabet_size, 
                                 concentration_1 = 0.1,
                                 concentration_2 = 0.1)
    ind_start <- which(df$segment == segment) %>% first()
    ind_end <- which(df$segment == segment) %>% last()
    prev_event <- if (segment == 1L) {
      # Sample the previous event from the steady state distribution
      sample(alphabet_size, size = 1, prob = steadyStates(mod) %>% pmax(0))
    } else {
      # Take the actual previous event
      df$symbol[ind_start - 1]
    }
    df$symbol[ind_start:ind_end] <-
      rmarkovchain(n = ind_end - ind_start + 1, 
                   object = mod, 
                   t0 = as.character(prev_event)) %>% as.integer()
  }
  
  df
}

generate_corpus <- function(corpus_opt) {
  map(seq_len(corpus_opt$num_seq),
      ~ generate_seq(alphabet_size = corpus_opt$alphabet_size, 
                     num_events = corpus_opt$num_events, 
                     change_prob = corpus_opt$change_prob))
}

new_ppm_par <- function(ltm_weight = 1,
                        ltm_half_life = 1e80,
                        stm_weight = 1, 
                        stm_duration = 0,
                        order_bound = 1) {
  unlist(as.list(environment()))
}

optim_ppm <- function(ppm,
                      alphabet_size,
                      corpus,
                      forget,
                      xtol_abs,
                      max_eval = 500,
                      ran_seed = 1) {
  stopifnot(!anyDuplicated(ppm$which_optim),
            all(ppm$which_optim %in% names(ppm$starting_par)),
            length(ppm$optim_lower) == length(ppm$which_optim),
            length(ppm$optim_higher) == length(ppm$which_optim))
  
  counter <- 1L
  eval_f <- function(par) {
    all_par <- ppm$starting_par
    all_par[ppm$which_optim] <- par
    cost <- eval_ppm_mod(ppm_par = all_par,
                         corpus = corpus,
                         forget = forget,
                         alphabet_size = alphabet_size) %>% mean()
    flog.info("i = %i, par = [%s], cost = %.5f...", 
              counter,
              sprintf("%.3f", par) %>% paste(collapse = ", "), 
              cost)
    counter <<- counter + 1L
    cost
  }
  
  with_log("Optimising PPM model...", {
    nloptr::nloptr(
      x0 = ppm$starting_par[ppm$which_optim],
      eval_f = eval_f,
      eval_grad_f = NULL,
      lb = ppm$optim_lower,
      ub = ppm$optim_higher,
      opts = list(algorithm = "NLOPT_LN_SBPLX",
                  maxeval = max_eval,
                  ranseed = ran_seed,
                  xtol_abs = xtol_abs)
    )
  })$solution %>%
    set_names(ppm$which_optim) %>% 
    insert_ppm_par(ppm$starting_par)
}

insert_ppm_par <- function(x, default_par = new_ppm_par()) {
  par <- default_par
  par[names(x)] <- x
  par
}

new_corpus_opt <- function(alphabet_size, 
                           num_seq = 50, 
                           num_events = 500,
                           change_prob = 0.01) {
  as.list(environment())
}

new_ppm_optim <- function(starting_par, 
                          which_optim, 
                          optim_lower, 
                          optim_higher) {
  as.list(environment())
}


run_exp <- function(alphabet_size = 5,
                    corpus_opt = new_corpus_opt(alphabet_size),
                    ppm_optim = list(
                      `+ Decay` = new_ppm_optim(
                        starting_par = new_ppm_par(ltm_half_life = 15,
                                                   ltm_weight = 1,
                                                   stm_weight = 100),
                        which_optim = c("ltm_half_life"),
                        optim_lower = c(0.01), 
                        optim_higher = c(1e90)
                      )
                    ),
                    forget = TRUE,
                    xtol_abs = 1e-3) {
  stopifnot(!any(names(ppm_optim) %in% c("Original", "seq_id")))
  
  train_corpus <- with_log("Generating training corpus...", 
                           generate_corpus(corpus_opt))
  
  optim_par <- 
    map(ppm_optim, optim_ppm, alphabet_size, train_corpus, forget, xtol_abs)
  
  all_par <- c(list(Original = new_ppm_par()), optim_par)
  stopifnot(is.list(all_par) & length(all_par) == 1 + length(optim_par))
  
  test_corpus <-  with_log("Generating test corpus...", 
                           generate_corpus(corpus_opt))
  
  with_log("Evaluating models...", {
    map(all_par, eval_ppm_mod, test_corpus, forget, alphabet_size) %>% 
      bind_cols() %>% 
      add_column(seq_id = seq_along(test_corpus), .before = 1)
  })
}


eval_ppm_mod <- function(ppm_par, corpus, forget, alphabet_size) {
  new_mod <- function() new_ppm_decay(alphabet_size = alphabet_size, 
                                      order_bound = ppm_par[["order_bound"]],
                                      ltm_weight = ppm_par[["ltm_weight"]], 
                                      ltm_half_life = ppm_par[["ltm_half_life"]],
                                      stm_weight = ppm_par[["stm_weight"]], 
                                      stm_duration = ppm_par[["stm_duration"]])
  
  mod <- new_mod()
  
  map_dbl(corpus, function(x) {
    if (forget) mod <- new_mod()
    model_seq(mod, 
              seq = x$symbol,
              time = seq_along(x$symbol),
              return_distribution = TRUE, 
              return_entropy = FALSE) %>% 
      mutate(correct = map2_lgl(symbol, distribution, ~ .x == which.max(.y)),
             incorrect = !correct) %>% 
      pull(incorrect) %>% 
      mean()
  })
}

with_log <- function(msg, expr) {
  futile.logger::flog.info(msg)
  start <- Sys.time()
  x <- force(expr)
  finish <- Sys.time()
  futile.logger::flog.info("...done after %s.", format(finish - start, digits = 2))
  x
}

plot_exp <- function(exp) {
  conditions <- setdiff(names(exp), "seq_id")
  cols <- viridis::viridis(n = 2, begin = 0.4, end = 1)
  exp %>% 
    gather(key = "condition", value = "error_rate", - seq_id) %>%
    mutate(accuracy = 1 - error_rate,
           condition = factor(condition, levels = conditions)) %>%
    ggplot(aes(x = condition, y = accuracy)) + 
    geom_boxplot(outlier.shape = NA, 
                 width = 0.3, size = 1.25, 
                 fatten = 1.5, colour="grey70") + 
    geom_point(colour = cols[1], shape = 21) +
    geom_line(aes(group = seq_id), alpha = 1, linetype = "dashed", colour = cols[1]) + 
    scale_x_discrete(NULL) + 
    scale_y_continuous("Accuracy") +
    theme(legend.position = "none",
          aspect.ratio = 1)
}
