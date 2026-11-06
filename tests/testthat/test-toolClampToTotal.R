mg <- function(mat, name = "x") {
  m <- new.magpie(rownames(mat), colnames(mat), name, fill = NA)
  m[, , ] <- as.vector(mat)
  m
}
m2 <- function(...) {
  mat <- cbind(...)
  dimnames(mat) <- list(c("AAA", "BBB"), paste0("y", 2000 + seq_len(ncol(mat)) - 1))
  mat
}

test_that("a sub-population above its species total is capped at the total", {
  sub   <- mg(m2(c(150,  50), c(80, 120)))
  total <- mg(m2(c(100, 200), c(90, 100)))
  out   <- as.vector(toolClampToTotal(sub, total, "test"))
  expect_equal(out, c(100, 50, 80, 100))
})

test_that("a reported 0 total means missing, so those cells are left unchanged", {
  sub   <- mg(m2(c(150, 50)))
  total <- mg(m2(c(0, 100)))
  expect_equal(as.vector(toolClampToTotal(sub, total, "test")), c(150, 50))
})

test_that("NA on either side is left alone", {
  sub   <- mg(m2(c(NA, 150), c(150, NA)))
  total <- mg(m2(c(100, 100), c(NA, 100)))
  out   <- as.vector(toolClampToTotal(sub, total, "test"))
  expect_true(is.na(out[1]))
  expect_equal(out[2], 100)
  expect_equal(out[3], 150)
  expect_true(is.na(out[4]))
})

test_that("structure is preserved", {
  sub   <- mg(m2(c(150, 50), c(80, 120)), name = "dairy goats")
  total <- mg(m2(c(100, 200), c(90, 100)))
  out   <- toolClampToTotal(sub, total, "test")
  expect_equal(dim(out), dim(collapseNames(sub)))
  expect_equal(getItems(out, dim = 1), getItems(sub, dim = 1))
  expect_equal(getYears(out), getYears(sub))
})
