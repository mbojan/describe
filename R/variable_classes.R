#' Columns of data frame by class
#'
#' @param dat data frame or object inheriting from one
#'
#' @return Returns a list with as many elements as there are classes of columns
#' in \code{dat}. Elements are named with the names of the classses. Each
#' element is a character vector of column names of \code{dat} of the
#' corresponding class.
#'
#' @export
variable_classes <- function(dat) {
  stopifnot(inherits(dat, "data.frame"))
  cls <- sapply(dat, class)
  vtypes <- lapply( unique(cls), function(x) names(cls)[cls==x] )
  names(vtypes) <- unique(cls)
  vtypes
}
