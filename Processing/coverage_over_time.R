coverage_over_time = function(counts){
  coverage_over_time = counts %>%
    group_by(zeroed) %>%
    summarize(total = sum(coverage,na.rm=T)/length(patients),
              mean = mean(coverage,na.rm=T),
              q10=quantile(coverage, 0.1, na.rm=T), median = quantile(coverage, 0.5, na.rm=T),
              q90=quantile(coverage, 0.9, na.rm=T))
}

