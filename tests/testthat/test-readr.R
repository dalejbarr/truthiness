## truthiness version 1.2.4 had two preprocessing functions,
## get_varnames() and scrape_cols() that used readr::read_csv()
## on a vector of CSV data; specifically,
##
## readr::read_csv(readLines(path)[-c(2:3)])
##
## In the readr 2.0.0, this behavior requires the I() operator on the
## input, e.g.
##
## readr::read_csv(I(readLines(path)[-c(2:3)]))
##
## the code below tests that the updated code works.
test_that("compatible with readr", {
  target <- c("col1", "col2", "col3", "col4")
  lines <- c(paste(target, collapse=","),
             "1,2,3,4",
             "2,3,4,5",
             "3,4,5,6")
  tf <- tempfile(fileext=".csv")
  writeLines(lines, tf)
  dat <- scrape_cols(tf, c("col2", "col3"))
  vnames <- get_varnames(tf)
  file.remove(tf)
  expect_true(identical(vnames, target) &&
              inherits(dat, "data.frame") &&
              identical(dim(dat), c(1L, 2L)))
})
