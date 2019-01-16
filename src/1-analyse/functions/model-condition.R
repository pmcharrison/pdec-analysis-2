model_condition <- function(alphabet_size, tone_len_ms, dat,
                            ppm_spec, change_point_spec, 
                            alphabet, downsample = NULL) {
  res <- with_seed(1, {
    trials <- get_trials(alphabet_size, tone_len_ms, dat, downsample)
    list(info = as.list(environment())[c("alphabet_size", "tone_len_ms", 
                                         "ppm_spec", "change_point_spec",
                                         "alphabet",
                                         "downsample")],
         res = plyr::alply(trials, 1, model_trial, 
                           ppm_spec = ppm_spec, 
                           change_point_spec = change_point_spec,
                           alphabet = alphabet,
                           .progress = "text")
    )
  })
  class(res) <- c("condition_analysis", class(res))
  res
}

get_trials <- function(alphabet_size, tone_len_ms, dat, downsample) {
  candidates <- dat %>% filter(alphabet_size == !!alphabet_size &
                                 tone_len_ms == !!tone_len_ms &
                                 cond == "randreg")
  stopifnot(nrow(candidates) > 0L)
  if (is.null(downsample)) {
    candidates
  } else {
    sample_n(candidates, size = downsample, replace = FALSE)
  }
}
