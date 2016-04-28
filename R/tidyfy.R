#' Tidy data frame
#'
#' @param x R object, usually a data frame
#' @param by character, variables names to split descriptives by
#' @param ... other arguments passed to/from other methods
#'
#' @return List of tidied data frames.
#' @export
#' @import dplyr tidyr

tidyfy <- function(x, ...) UseMethod("tidyfy")



#' @method tidyfy grouped_df
#' @rdname tidyfy
#' @export
tidyfy.grouped_df <- function(x, ...) {
  # names of grouping variables
  by <- as.character(attr(x, "vars"))
  NextMethod("tidyfy", x, by=by, ...)
}




#' @method tidyfy data.frame
#' @rdname tidyfy
#' @export
tidyfy.data.frame <- function(x, by, ...) {
  stopifnot(is.character(by))
  vtypes <- variable_classes(x)
  # List of data frames
  datlist <- lapply(vtypes, function(vn) {
    s <- dplyr::select_(x, lazyeval::interp(~one_of(v), v=unique(c(by, vn))))
    tidyr::gather_(s, "key", "value", setdiff(names(s), by))
  } )
  datlist
}


#' @method tidyfy default
#' @rdname tidyfy
#' @export
tidyfy.default <- function(x, ...) {
  stop("Unsupported class of `x`: ", paste(class(x), sep=", "))
}
