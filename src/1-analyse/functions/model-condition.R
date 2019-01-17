model_condition <- function(alphabet_size, tone_len_ms, dat,
                            ppm_spec, change_point_spec, 
                            alphabet, downsample = NULL) {
  with_seed(1, {
    trials <- get_trials(alphabet_size, tone_len_ms, dat, downsample)
    flog.info(glue("Analysing {nrow(trials)} trials with ", 
                   "alphabet_size = {alphabet_size} and ",
                   "tone_len_ms = {tone_len_ms}..."))
    out <- list(
      info = as.list(environment())[c("alphabet_size", "tone_len_ms", 
                                      "ppm_spec", "change_point_spec",
                                      "alphabet",
                                      "downsample")],
      res = plyr::alply(trials, 1, model_trial, 
                        ppm_spec = ppm_spec, 
                        change_point_spec = change_point_spec,
                        alphabet = alphabet,
                        .progress = "text") %>% unname()
    )
    attr(out$res, "split_type") <- NULL
    attr(out$res, "split_labels") <- NULL
    class(out) <- c("condition_analysis", class(out))
    out
  })
}

print.condition_analysis <- function(x, ...) {
  cat(
    "An object of class 'condition_analysis':\n",
    "- alphabet size = ", x$info$alphabet_size, "\n",
    "- tone_len_ms = ", x$info$tone_len_ms, "\n",
    "- num sequences = ", length(x$res), "\n",
    sep = ""
  )
}

get_trials <- function(alphabet_size, tone_len_ms, dat, downsample) {
  fake_tone_len_ms <- TRUE
  # if (fake_tone_len_ms) warning("Faking tone length")
  candidates <- dat %>% filter(
    alphabet_size == !!alphabet_size &
      (fake_tone_len_ms | (tone_len_ms == !!tone_len_ms)) &
      cond == "randreg"
  )
  if (fake_tone_len_ms) candidates$tone_len_ms <- tone_len_ms

  stopifnot(nrow(candidates) > 0L)
  if (is.null(downsample)) {
    candidates
  } else {
    sample_n(candidates, size = downsample, replace = FALSE)
  }
}

