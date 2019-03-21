get_ppm_spec <- function() {
  read_csv("input/ppm-spec.csv", col_types = cols()) %>% 
    filter(include) %>% 
    mutate(i = seq_len(n())) %>% 
    select(- include) %>% 
    select(i, everything())
}

ppm_options_from_ppm_spec <- function(x) {
  if (is.data.frame(x)) stopifnot(nrow(x) == 1L)
  x
}
