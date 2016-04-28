# Descriptive table
#
# Input = the result of group_by, or with `by` argument

library(dplyr)
library(tidyr)

# Test data
data(mtcars)
d <- mtcars %>%
  mutate(
    name = rownames(mtcars),
    gear = factor(gear),
    vs = ifelse(vs == 1, "yes", "no"),
    l = letters[gear],
    am = am == 1
  )
str(d)

dg <- d %>% group_by(cyl)


# Variables by type
variable_types <- function(dat) {
  stopifnot(inherits(dat, "data.frame"))
  cls <- sapply(dat, class)
  vtypes <- lapply( unique(cls), function(x) names(cls)[cls==x] )
  names(vtypes) <- unique(cls)
  vtypes
}

# Variable types

x <- lapply(vtypes, function(vn) select_(dg, lazyeval::interp(~one_of(x), x=vn)))
str(x)

#' Return a list of tidy data frames by variable type
tidyfy <- function(dat) {
  vtypes <- variable_types(dat)
  gvars <- as.character(attr(dat, "vars"))
  # List of data frames
  datlist <- lapply(vtypes, function(vn) {
    s <- select_(d, lazyeval::interp(~one_of(x), x=unique(c(gvars, vn))))
    gather_(s, "key", "value", setdiff(names(s), gvars))
  } )
  datlist
}

x <- tidyfy(dg)
str(x)




# Numeric

num <- d %>%
  select( cyl, one_of(vtypes$numeric)) %>%
  gather(key, value, -cyl)

snum <- num %>% group_by(cyl, key) %>%
  summarise(
    m = mean(value),
    s = sd(value)
  ) %>%
  ungroup() %>%
  mutate( ch = paste0( round(m, 1), " (+-", round(s, 1), ")")) %>%
  select(cyl, key, ch) %>%
  spread(cyl, ch)


# Character


ch <- d %>%
  select(cyl, one_of(vtypes$character), -name) %>%
  gather(key, value, -cyl)

sch <- ch %>% group_by(cyl, key, value) %>%
  summarise(n=n() ) %>%
  ungroup() %>%
  group_by(cyl, key) %>%
  mutate( pct = n / sum(n) * 100) %>%
  ungroup() %>%
  mutate(ch = paste0( n, " (", round(pct, 1), "%)")) %>%
  select(cyl, key, value, ch  ) %>%
  spread(cyl, ch)

# Logical






# Combine

rbind(mutate(snum, value=NA), sch) %>%
  select(key, value, everything())
