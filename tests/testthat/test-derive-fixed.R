test_that("param estimates match", {
  testtib <- cbind(int = rep(1, 8),
                   R = rep(c(-.5, .5), each = 4),
                   I1 = rep(rep(c(-1/4, 3/4, -1/4), c(1, 1, 2)), 2),
                   I2 = rep(rep(c(-1/4, 3/4, -1/4), c(2, 1, 1)), 2),
                   I3 = rep(rep(c(-1/4, 3/4), c(3, 1)), 2))
  tt <- cbind(testtib,
              `R:I1` = testtib[, "R"] * testtib[, "I1"],
              `R:I2` = testtib[, "R"] * testtib[, "I2"],
              `R:I3` = testtib[, "R"] * testtib[, "I3"])

  ff <- 1:4
  result <- as.vector(tt %*% derive_fixed(ff))
  expect_equal(result[5:8] - result[1:4], ff)

  ff <- c(0, .14, .14, .14)
  result <- as.vector(tt %*% derive_fixed(ff))
  expect_equal(result[5:8] - result[1:4], ff)

  ff <- c(0, 0, 0, .14)
  result <- as.vector(tt %*% derive_fixed(ff))
  expect_equal(result[5:8] - result[1:4], ff)

  ff <- c(.14, 0, 0, 0)
  result <- as.vector(tt %*% derive_fixed(ff))
  expect_equal(result[5:8] - result[1:4], ff)
})
