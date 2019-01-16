get_alphabet <- function(dat) {
  dat$stim %>% bind_rows() %>% pull(tone) %>% unique() %>% sort()
}
