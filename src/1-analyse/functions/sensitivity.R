get_sensitivity_datasets <- function(dat, n, sd_rel, downsample = NULL, fake_tone_len_ms = FALSE) {
  original_ppm_spec <- get_ppm_spec() %>% filter(label == "L4 + buffer 15")
  stopifnot(nrow(original_ppm_spec) == 1)
  
  cols_to_resample <- c(
    "buffer_length_items",
    "buffer_weight",
    # (STM isn't used here)
    "ltm_half_life",
    "ltm_weight",
    "noise"
  )
  stopifnot(all(cols_to_resample %in% names(original_ppm_spec)))
  
  with_seed(1, {
    ppm_spec <- map_dfr(seq_len(n), safe_resample_ppm_spec, original_ppm_spec, sd_rel, cols_to_resample)
  })
  
  model(dat, downsample = downsample, fake_tone_len_ms = fake_tone_len_ms, ppm_spec = ppm_spec)
}

safe_resample_ppm_spec <- function(i, original_ppm_spec, sd_rel, cols_to_resample) {
  counter <- 0
  while(TRUE) {
    res <- resample_ppm_spec(i, original_ppm_spec, sd_rel, cols_to_resample) 
    if (res$buffer_length_items > 0 && 
        res$buffer_weight > 0 &&
        res$ltm_half_life > 0 &&
        res$ltm_weight > 0 && res$ltm_weight <= res$buffer_weight &&
        res$noise >= 0) {
      return(res)
    }
    
    counter <- counter + 1
    if (counter > 1e3) {
      stop("couldn't find a valid resampled PPM specification")
    }
  }
}

resample_ppm_spec <- function(i, original_ppm_spec, sd_rel, cols_to_resample) {
  new <- original_ppm_spec
  for (col in cols_to_resample) {
    original_val <- new[[col]]
    sd_abs <- sd_rel * original_val
    new_val <- rnorm(1, mean = original_val, sd = sd_abs)
    new[[col]] <- new_val
  }
  new$stm_weight <- new$buffer_weight
  new$buffer_length_items <- round(new$buffer_length_items)
  new$i <- i
  new$label <- paste("Resample", new$i)
  new
}
