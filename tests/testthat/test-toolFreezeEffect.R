populationMagpie <- magclass::maxample("pop")

test_that("Expect last year after freezing to first year to be equivalent to first year", {
  pFirst       <- populationMagpie[, 1, ]
  pFreezeLast <- toolFreezeEffect(populationMagpie, 1)[, length(getYears(populationMagpie)), ]
  expect_equal(setYears(pFreezeLast, NULL), setYears(pFirst, NULL), ignore_attr = TRUE)
})

test_that("Expect first non-zero use constrain to fix values after freeze year", {

  d  <- new.magpie(1:2, 1:2, 1:2, rep(rep(c(0, 1), each = 2), times = 2))
  d2 <- toolFreezeEffect(d, 1, constrain = "first_use")

  expect_identical(d,  d2)
})
