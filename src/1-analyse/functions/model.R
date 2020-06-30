#' @param downsample Number of trials to downsample to within each condition
#' (NULL to disable downsampling)
#' @param fake_tone_len_ms See get_trials().
model <- function(dat, downsample = NULL, fake_tone_len_ms = FALSE, ppm_spec = get_ppm_spec()) {
  alphabet <- get_alphabet(dat)
  change_point_spec <- get_change_point_spec()
  ppm_spec$res <- plyr::alply(ppm_spec, 1, model_conditions, 
                              dat, change_point_spec, alphabet, 
                              downsample, fake_tone_len_ms)
  ppm_spec
}
