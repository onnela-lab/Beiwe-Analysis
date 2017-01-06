get_days = function(...){
  # input:  any number of dataframe objects with a "days" column.
  # output: a vector of all the unique days.
  list(...) %>%
    lapply(function(x) x[,'days']) %>%
    Reduce(union, .) %>%
    unique %>%
	sort %>%
	setdiff("NA")
}