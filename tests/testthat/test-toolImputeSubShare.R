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

test_that("a genuine gap is imputed from the most recent real share", {
  raw   <- mg(m2(c(50, 10), c(NA, 20), c(NA, 30)))
  sub   <- mg(m2(c(50, 10), c(0, 20), c(0, 30)))
  total <- mg(m2(c(100, 100), c(200, 100), c(300, 100)))
  out   <- toolImputeSubShare(sub, total, raw)
  expect_equal(as.vector(out["AAA", , ]), c(50, 100, 150))
  expect_equal(as.vector(out["BBB", , ]), c(10, 20, 30))
})

test_that("a reported 0 is never overwritten", {
  raw   <- mg(m2(c(50, 10), c(0, 20), c(NA, 30)))
  sub   <- mg(m2(c(50, 10), c(0, 20), c(0, 30)))
  total <- mg(m2(c(100, 100), c(200, 100), c(300, 100)))
  expect_equal(as.vector(toolImputeSubShare(sub, total, raw)["AAA", , ]), c(50, 0, 150))
})

test_that("an imputed value is capped at that year's total, a reported one is not", {
  raw   <- mg(m2(c(150, 10), c(NA, 20)))
  sub   <- mg(m2(c(150, 10), c(0, 20)))
  total <- mg(m2(c(100, 100), c(200, 100)))
  out   <- as.vector(toolImputeSubShare(sub, total, raw)["AAA", , ])
  expect_equal(out[2], 200)  # 1.5 * 200 capped at the total
  expect_equal(out[1], 150)  # reported above its own total; toolClampToTotal handles that
})

test_that("a gap with no earlier real observation is left unresolved", {
  raw   <- mg(m2(c(NA, 10), c(50, 20)))
  sub   <- mg(m2(c(0, 10), c(50, 20)))
  total <- mg(m2(c(100, 100), c(200, 100)))
  expect_equal(as.vector(toolImputeSubShare(sub, total, raw)["AAA", , ]), c(0, 50))
})

test_that("a country with no real share evidence at all is untouched", {
  raw   <- mg(m2(c(NA, 10), c(NA, 20)))
  sub   <- mg(m2(c(0, 10), c(0, 20)))
  total <- mg(m2(c(100, 100), c(200, 100)))
  expect_equal(as.vector(toolImputeSubShare(sub, total, raw)["AAA", , ]), c(0, 0))
})
