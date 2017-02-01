moving_average = function(x, n_neighbors=1){
  l = length(x)
  y=c(rep(x[1],n_neighbors), x, rep(x[l],n_neighbors))
  z = rep(0,length(x))
  for(i in 0:(n_neighbors * 2)) z = z + y[1:l+i]
  return(as.numeric(z/(n_neighbors*2+1)))
}
