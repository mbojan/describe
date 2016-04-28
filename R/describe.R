#' Descriptive statistics
#'
#' @param x data frame
#' @param ... other arguments passed to/from other methods
#'
#' @return Data frame with summaries
#' @export
#'
#' @examples
#' library(dplyr)
#' mtcars %>% group_by(cyl) %>% describe()
#'
describe <- function(x, ...) {
  td <- tidyfy(x)
  describe_numeric(td$numeric, ...)
}

# Describe tidy data frame of numeric variables
describe_numeric <- function(x, ...) {
  x %>%
    summarise_(
      m=lazyeval::interp(~mean(v), v=as.name("value")),
      s=lazyeval::interp(~sd(v), v=as.name("value"))
    ) %>%
    ungroup() %>%
    mutate( ch = paste0( round(m, 1), " (+-", round(s, 1), ")"))
  # %>%
  #   select_(cyl, key, ch) %>%
  #   spread_(cyl, ch)
}

