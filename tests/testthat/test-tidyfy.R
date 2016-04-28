context("Tidyfy works on data frames")

test_that("tidyfy works on data frames", {
  data(mtcars)
  mtcars$name <- rownames(mtcars)

  r <- tidyfy(mtcars, by="cyl")
  str(r)
})
