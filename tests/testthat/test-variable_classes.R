context("Test variable_classes")

test_that("variable_classes works correctly on `mtcars`", {
  # Add row names as a separate column
  data(mtcars)
  mtcars$name <- rownames(mtcars)
  r <- variable_classes(mtcars)

  # Returns a list
  expect_type(r, "list")

  # Elements are vectors of variable names
  expect_true( all( sapply(r, class) == "character" ) )

  vnames <- names(mtcars)
  l1 <- lapply(r, function(v) vnames %in% v )
  m <- do.call("rbind", l1)
  # All variables in df can be found in `r`
  expect_true( all(apply(m, 2, any)) )

  # ... and only once
  expect_true( all(colSums(m) == 1) )

  # All variable names in `r` appear in `df`
  l2 <- lapply(r, function(v) v %in% vnames )
  expect_true( all(sapply(l2, all))   )
})
