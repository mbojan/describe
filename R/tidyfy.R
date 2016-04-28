
tidyfy <- function(x, ...) UseMethod("tidyfy")


tidyfy.grouped_df <- function(x, ...) {
  # names of grouping variables
  by <- as.character(attr(dat, "vars"))
  NextMethod("tidyfy", object=x, by=by)
}

tidyfy.data.frame <- function(x, by, ...) {
  stopifnot(is.character(by))
  vtypes <- variable_classes(x)
  # List of data frames
  datlist <- lapply(vtypes, function(vn) {
    s <- select_(x, lazyeval::interp(~one_of(v), v=unique(c(by, vn))))
    gather_(s, "key", "value", setdiff(names(s), by))
  } )
  datlist
}

