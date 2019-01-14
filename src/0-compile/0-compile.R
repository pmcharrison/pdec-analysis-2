for (f in list.files("src/0-compile/functions/", full.names = TRUE))
  source(f)

library(testthat)

warning("Participant 7 needs to be excluded")

dat <- list(all = tidy_all(), 
            stim = tidy_stim(), 
            stim_desc = tidy_stim_desc())
check_1(dat)

dat$all$stim <- dat$stim_desc %>% 
  {split(dat$stim_desc, cumsum(c(TRUE, diff(.$tone_num) < 0)))}

check_2(dat)

dat$all <- add_column(dat$all, 
                      transition = map_int(dat$all$stim, ~ unique(.$transition)), 
                      .before = "stim")

dat$all$stim <- map2(dat$all$stim, do.call(c, dat$stim$data), function(x, y) {
  mutate(x, tone = y) %>% 
    select(tone_num, transition, tone)
})

dat$all <- select(dat$all, - c(speed_i, cond_i))

dat$all <- add_column(dat$all, row_id = seq_len(nrow(dat$all)), .before = 1L)

warning("need to drop excluded trials (include == FALSE)")
warning("only keep condi 2 and 4 for baselining")

rt_baselines <- dat$all %>% 
  group_by(subj, block) %>% 
  filter(cond == "step" & response == "hit" & include) %>% 
  summarise(rt_reference = mean(rt))

dat$all <- left_join(dat$all, rt_baselines, by = c("subj", "block"))

dat$all <- dat$all %>% mutate(
  rt_norm = rt - rt_reference,
  rt_norm_tones = 1000 * rt_norm / tone_len_ms
)

rt_baselines_2 <- dat$all %>% 
  group_by(block, tone_len_ms, alphabet_size, cond_i) %>% 
  summarise(mean = mean(rt_norm, na.rm = TRUE),
            sd = sd(rt_norm, na.rm = TRUE))
  
warning("need to drop cases where rt_norm > 2SD from mean in rt_baselines_2")
  

saveRDS(dat$all, "dat-response.rds")
