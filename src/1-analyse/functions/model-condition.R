model_condition <- function(alphabet_size, tone_len_ms, dat,
                            ppm_spec, change_point_spec, 
                            alphabet, downsample, fake_tone_len_ms) {
  with_seed(1, {
    trials <- get_trials(alphabet_size, tone_len_ms, dat, downsample, fake_tone_len_ms)
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

R.utils::mkdirs("~/Downloads/cache")
model_condition <- memoise(model_condition, cache = cache_filesystem("~/Downloads/cache"))

print.condition_analysis <- function(x, ...) {
  cat(
    "An object of class 'condition_analysis':\n",
    "- alphabet size = ", x$info$alphabet_size, "\n",
    "- tone_len_ms = ", x$info$tone_len_ms, "\n",
    "- num sequences = ", length(x$res), "\n",
    sep = ""
  )
}

#' @param fake_tone_len_ms If \code{TRUE},
#' then sequences are artificially matched between tone lengths.
#' This helps comparisons between different tone lengths converge faster,
#' but it is not quite a veridical account of the data.
get_trials <- function(alphabet_size, tone_len_ms, dat, downsample, 
                       fake_tone_len_ms) {
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
