for (f in list.files("src/0-compile/functions/", full.names = TRUE))
  source(f)

library(testthat)

dat <- get_raw() # get raw data from text files
check_1(dat)
dat <- merge_in_stim_desc(dat) # merge dat$stim_desc into dat$all
if (FALSE) check_2(dat) # this is slow, taking about a minute
dat <- merge_in_stim(dat) # merge dat$stim into dat$all
dat <- drop_supplementary(dat) # drop supplementary data objects
dat <- add_row_id(dat) 
dat <- drop_excluded_subj(dat, subj_to_exclude = c(7))
dat <- drop_excluded_trials(dat) 
rt_baselines_1 <- get_rt_baselines_1(dat) # used to normalise reaction times; benchmarked within subjects and blocks
dat <- norm_reaction_times(dat, rt_baselines_1) # note: only randreg trials are normalised
rt_baselines_2 <- get_rt_baselines_2(dat) # used to discard outliers; benchmarked within conditions, across subjects, across blocks
dat <- drop_outliers(dat, rt_baselines_2, z_threshold = 2)
regression_test(dat)
saveRDS(dat, "output/dat-response.rds")
