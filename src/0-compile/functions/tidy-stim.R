tidy_stim <- function() {
  dir <- "input/stimuli"
  tibble::tibble(
    file_name = list.files(dir),
    path = file.path(dir, file_name),
    subj = file_name %>% 
      gsub("^stimuli_subj_", "", .) %>% 
      gsub("_exp2_stm.txt$", "", .) %>% 
      as.integer
  ) %>% 
    arrange(subj) %>% 
    mutate(data = map(path, readLines),
           data = map(data, strsplit, " "),
           data = map(data, ~ map(., as.integer))) %>% 
    select(subj, data)
}
