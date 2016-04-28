context("Tidyfy stops on unsupported classes")

test_that("tidyfy stops on vector", {
  expect_error( tidyfy(1:5) )
})







context("Does tidyfy work at all")


test_that("tidyfy.data.frames returns a list of data frames", {
  data(mtcars)
  mtcars$name <- rownames(mtcars)

  r <- tidyfy(mtcars, by=c("cyl", "hp"))
  expect_true( all( sapply(r, inherits, "data.frame", which=TRUE) == 1) )
  # str(r)
})



test_that("tidyfy.grouped_df returns a list of group_df's", {
  data(mtcars)
  mtcars$name <- rownames(mtcars)
  dt <- as.tbl(mtcars)
  gd <- dplyr::group_by_(dt, .dots=c("cyl", "hp") )
  r <- tidyfy(gd)
  expect_true( all(sapply(r, inherits, "grouped_df", which=TRUE) == 1 ))
})
