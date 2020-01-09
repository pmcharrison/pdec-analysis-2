library(broom)

do_stats_subj <- function(subj_vals) {
  raw <- subj_vals %>% 
    group_by(alphabet_size, tone_len_ms) %>% 
    do(data = (.)) %>% 
    ungroup() %>% 
    add_column(group = NA, .before = 1) %>% 
    mutate(group = seq_len(n()))
  
  list(
    wilcox = wilcox_subj(raw),
    friedman = friedman_subj(raw),
    anova = do_anova(raw),
    condition_differences = get_condition_differences(raw)
  )
}

do_anova <- function(raw) {
  bind_rows(raw$data) %>%
    mutate_at(c("subj", "alphabet_size", "tone_len_ms"), factor) %>% 
    ez::ezANOVA(dv = subj_val,
                wid = subj,
                within = .(alphabet_size, tone_len_ms),
                type = 3,
                detailed = TRUE,
                return_aov = TRUE)
    # schoRsch::anova_out(ezout = ., print = TRUE, sph.cor = "GG", mau.p = 0.05, etasq = "partial", dfsep = ", ")
}

get_condition_differences <- function(raw) {
  pairwise <- get_pairwise_condition_differences(raw)
  interaction <- get_interaction(pairwise)
  list(
    pairwise = pairwise,
    interaction = interaction
  )
}

get_interaction <- function(x) {
  df <- c(alph_10 = 10,
          alph_20 = 20) %>% 
    map(function(alph_size) 
      filter(x, tone_len_ms == 75 & alphabet_size == alph_size))
  stopifnot(!anyDuplicated(df[[1]]$subj),
            all(df[[1]]$subj == df[[2]]$subj))
  data <- tibble(subj = df[[1]]$subj,
                 value = df$alph_20$diff - df$alph_10$diff)
  set.seed(1)
  bootstrap <- boot::boot(data$value, function(data, ind) mean(data[ind]), R = 1e5)
  conf_int <- boot::boot.ci(bootstrap, type = "bca")
  list(data = data,
       conf_int = conf_int$bca[, 4:5])
}

get_pairwise_condition_differences <- function(raw) {
  alph_sizes <- unique(raw$alphabet_size)
  tone_lengths <- unique(raw$tone_len_ms)
  ref_tone_len_ms <- 25
  expand.grid(alphabet_size = alph_sizes,
              tone_len_ms = setdiff(tone_lengths, ref_tone_len_ms)) %>% 
    as_tibble() %>% 
    pmap(function(alphabet_size, tone_len_ms) {
      difference_conditions(
        get_from_raw(raw, alphabet_size, tone_len_ms),
        get_from_raw(raw, alphabet_size, ref_tone_len_ms)
      ) %>% 
        add_column(tone_len_ms = tone_len_ms, .before = 1) %>% 
        add_column(ref_tone_len_ms = ref_tone_len_ms, .before = 1) %>% 
        add_column(alphabet_size = alphabet_size, .before = 1)
    }) %>% 
    bind_rows()
}

difference_conditions <- function(x, y) {
  stopifnot(all(x$subj == y$subj),
            !anyDuplicated(x$subj))
  tibble(subj = x$subj,
         diff = x$subj_val - y$subj_val)
}

get_from_raw <- function(raw, alphabet_size, tone_len_ms) {
  raw %>% 
    filter(alphabet_size == !!alphabet_size & 
             tone_len_ms == !!tone_len_ms) %>%
    pull(data) %>% `[[`(1)
}

friedman_subj <- function(raw) {
  x <- raw$data %>% bind_rows() %>% split(., .$subj)
  map(c(`10` = 10, `20` = 20), function(alph_size) {
    map(x, function(subj_dat) {
      c(25, 50, 75) %>% set_names(., .) %>% 
        map(function(tone_len) {
          subj_dat %>% 
            filter(alphabet_size == alph_size & 
                     tone_len_ms == tone_len) %>%
            pull(subj_val)
        }) %>% as_tibble()
    }) %>% bind_rows() %>% as.matrix() %>% friedman.test() %>% glance() %>% 
      add_column(alphabet_size = alph_size, .before = 1)
  }) %>% bind_rows() %T>% write_csv("output/stats-subj-friedman.csv")
}

wilcox_subj <- function(raw) {
  gtools::combinations(n = nrow(raw), r = 2, v = raw$group) %>% 
    as.data.frame() %>% 
    set_names(c("group_1", "group_2")) %>% 
    as_tibble() %>% 
    mutate(dat_1 = map(group_1, ~ select(filter(raw, group == .)$data[[1]], 
                                         subj,
                                         subj_val)),
           dat_2 = map(group_2, ~ select(filter(raw, group == .)$data[[1]], 
                                         subj,
                                         subj_val)),
           dat = map2(dat_1, dat_2, inner_join, by = "subj")) %>% 
    select(- c(dat_1, dat_2)) %>% 
    mutate(
      wilcox = map(dat, ~ wilcox.test(x = .$subj_val.x,
                                      y = .$subj_val.y,
                                      paired = TRUE) %>% broom::glance()
      )) %>% select(- dat) %>% unnest() %>% 
    merge(select(raw, group, alphabet_size, tone_len_ms),
          by.x = "group_1", by.y = "group") %>% 
    merge(select(raw, group, alphabet_size, tone_len_ms),
          by.x = "group_2", by.y = "group",
          suffixes = c("_1", "_2")) %>% 
    as_tibble() %T>% 
    write_csv("output/stats-subj-wilcox.csv")
}
