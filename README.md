# `dplyr`-friendly tables of descriptive statistics

To be used like this:

```r
dataframe %>% group_by(variable) %>% describe()
```

or

```r
dataframe %>% describe(by="varname")
```

or

```r
describe(dataframe, by="varname")
```

