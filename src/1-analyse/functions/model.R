#' @param downsample Number of trials to downsample to within each condition
#' (NULL to disable downsampling)
#' @param fake_tone_len_ms See get_trials().
model <- function(dat, downsample = NULL, fake_tone_len_ms = FALSE) {
  alphabet <- get_alphabet(dat)
  change_point_spec <- get_change_point_spec()
  x <- get_ppm_spec()
  x$res <- plyr::alply(x, 1, model_conditions, 
                       dat, change_point_spec, alphabet, 
                       downsample, fake_tone_len_ms)
  x
}
