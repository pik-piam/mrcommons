context("Freezing values on fix year or first non-zero values")

population_magpie <- magclass::maxample("pop")

test_that("Expect last year after freezing to first year to be equivalent to first year", {
  p_first       <- population_magpie[,1,]
  p_freeze_last <- toolFreezeEffect(population_magpie,1)[,length(getYears(population_magpie)),]
  expect_equal(setYears(p_freeze_last, NULL), setYears(p_first, NULL), ignore_attr = TRUE)
})

test_that("Expect first non-zero use constrain to fix values after freeze year", {

  d  <- new.magpie(1:2,1:2,1:2,rep(rep(c(0,1), each=2), times=2))
  d2 <- toolFreezeEffect(d,1 ,constrain="first_use")
  
  expect_identical(d,  d2)
})