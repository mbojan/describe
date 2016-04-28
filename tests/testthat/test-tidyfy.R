context("Tidyfy works on data frames")

test_that("tidyfy works on data frames", {
  data(mtcars)
  r <- tidyfy(mtcars)
})
