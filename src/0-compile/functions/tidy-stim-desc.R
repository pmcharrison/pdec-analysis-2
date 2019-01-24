library(magrittr)

tidy_stim_desc <- function() {
  dir <- "input/stimDesc"
  tibble::tibble(
    file_name = list.files(dir, pattern = "\\.txt$"),
    path = file.path(dir, file_name),
    subj = file_name %>% 
      gsub("^stimDescription_subj_", "", .) %>% 
      gsub("_exp2_stm.txt$", "", .) %>% 
      as.integer
  ) %>% 
    arrange(subj) %>% 
    mutate(
      data = map2(subj, path, ~ add_column(read_stim_desc_file(.y), 
                                           subj = .x, 
                                           .before = 1L))
    ) %>% 
    pull(data) %>% 
    bind_rows()
}

read_stim_desc_file <- function(x) {
  all_lines <- read_lines(x) %>% 
    gsub(" ", "", .) %>%
    strsplit("\t")
  head <- all_lines[[1]]
  all_lines[-1] %>% 
    Filter(function(x) length(x) > 0, .) %T>%
    {stopifnot(all(map_int(., length) == 5))} %>% 
    map(as.integer) %>% 
    do.call(c, .) %>% 
    matrix(ncol = 5L, byrow = TRUE) %>% 
    as_tibble() %>% 
    set_names(head) %>% 
    rename(
      speed_i = speed_alphabet,
      cond_i = condition,
      tone_num = toneNum
    ) %>% 
    mutate(transition = if_else(transition == 0,
                                as.integer(NA), 
                                transition))
}
