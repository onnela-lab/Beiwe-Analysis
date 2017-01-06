warp_GPS = function(mobmat, num_warps = 10, seed = NA){
  # wrapper function, using warps on data.  Calls: warps.
  # input: mobmat, a dataframe with at least "x0", "x1", "y0", and "y1".
  # output: same thing, with warped GPS coordinates.
  warp_matrix = matrix(c(runif(num_warps, min(mobmat[,c("x0","x1")],na.rm=T), max(mobmat[,c("x0","x1")],na.rm=T)),
                         runif(num_warps, min(mobmat[,c("y0","y1")],na.rm=T), max(mobmat[,c("y0","y1")],na.rm=T))),num_warps,2)
  pushes = rep(c(1,0),length.out=num_warps)
  sds = 2*apply(mobmat[,c("x0","y0")], 2, sd, na.rm=TRUE)
  mobmat[,c("x0","y0")] = warps(mobmat[,c("x0","y0")], warp_matrix, pushes, sd1=sds[1],sd2=sds[2], seed)
  mobmat[,c("x1","y1")] = warps(mobmat[,c("x1","y1")], warp_matrix, pushes, sd1=sds[1],sd2=sds[2], seed)
  return(mobmat)
}