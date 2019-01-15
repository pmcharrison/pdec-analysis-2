get_cond <- function(dat) {
  dat %>% group_by(alphabet_size, tone_len_ms) %>% summarise() %>% ungroup()
}
