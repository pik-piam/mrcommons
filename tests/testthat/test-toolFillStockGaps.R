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

test_that("a gap with real values on both sides is interpolated", {
  stock <- mg(m2(c(100, 10), c(NA, 20), c(NA, 30), c(400, 40)))
  prod  <- mg(m2(c(1, 1), c(1, 1), c(1, 1), c(1, 1)))
  out   <- toolFillStockGaps(stock, prod)
  expect_equal(as.vector(out["AAA", , ]), c(100, 200, 300, 400))
  expect_equal(as.vector(out["BBB", , ]), c(10, 20, 30, 40))
})

test_that("a trailing gap is carried forward only where production says the animals exist", {
  stock <- mg(m2(c(100, 10), c(200, 20), c(NA, NA), c(NA, NA)))
  prod  <- mg(m2(c(1, 1), c(1, 1), c(5, 0), c(0, 0)))
  out   <- toolFillStockGaps(stock, prod)
  expect_equal(as.vector(out["AAA", , ]), c(100, 200, 200, 0))
  expect_equal(as.vector(out["BBB", , ]), c(10, 20, 0, 0))
})

test_that("a single-year edge gap is filled without production evidence", {
  stock <- mg(m2(c(NA, 10), c(200, 20), c(300, 30), c(400, 40)))
  prod  <- mg(m2(c(0, 0), c(0, 0), c(0, 0), c(0, 0)))
  expect_equal(as.vector(toolFillStockGaps(stock, prod)["AAA", , ]), c(200, 200, 300, 400))
})

test_that("a reported 0 is left as a reported 0", {
  stock <- mg(m2(c(100, 10), c(0, 20), c(300, 30), c(400, 40)))
  prod  <- mg(m2(c(1, 1), c(1, 1), c(1, 1), c(1, 1)))
  expect_equal(as.vector(toolFillStockGaps(stock, prod)["AAA", , ]), c(100, 0, 300, 400))
})

test_that("a series FAO never tracked comes back as 0", {
  stock <- mg(m2(c(NA, 10), c(NA, 20), c(NA, 30), c(NA, 40)))
  prod  <- mg(m2(c(1, 1), c(1, 1), c(1, 1), c(1, 1)))
  expect_equal(as.vector(toolFillStockGaps(stock, prod)["AAA", , ]), c(0, 0, 0, 0))
})
