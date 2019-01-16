model <- function(dat, downsample) {
  alphabet <- get_alphabet(dat)
  change_point_spec <- get_change_point_spec()
  x <- get_ppm_spec()
  x$res <- plyr::alply(x, 1, model_conditions, dat, change_point_spec, alphabet, downsample)
  x
}
