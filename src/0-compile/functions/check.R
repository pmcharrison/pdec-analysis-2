library(testthat)

check_1 <- function(dat) {
  check_subj(dat)
}

check_subj <- function(dat) {
  x <- dat$all$subj %>% unique %>% sort
  y <- dat$stim$subj
  z <- dat$stim_desc$subj %>% unique %>% sort
  x %T>% expect_equal(y) %T>% expect_equal(z)
  invisible(TRUE)
}

check_2 <- function(dat) {
  x <- dat$all
  plyr::l_ply(seq_len(nrow(x)), function(i) {
    y <- x$stim[[i]]
    select(y, subj, block, speed_i, cond_i) %>% 
      unique %T>%
      {stopifnot(nrow(.) == 1L)} %>% 
      expect_equal(x[i, ] %>% select(subj, block, speed_i, cond_i))
    expect_equal(y$tone_num, seq(from = 1, length.out = nrow(y)))
  }, .progress = "text")
  invisible(TRUE)
}
