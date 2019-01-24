model_conditions <- function(ppm_spec,
                             dat, 
                             change_point_spec,
                             alphabet, 
                             downsample) {
  flog.info(glue("Applying PPM variant '{ppm_spec$label}'..."))
  ppm_spec <- as.list(ppm_spec)
  ppm_spec$label <- NULL
  ppm_spec$group <- NULL
  cond <- get_cond(dat)
  cond %>% 
    mutate(detail = map2(alphabet_size, tone_len_ms,
                         model_condition,
                         dat, ppm_spec, change_point_spec, alphabet, downsample),
           lag_tones = map(detail, 
                           ~ map_int(.$res, ~ .$change_point$lag_tones)))
}
