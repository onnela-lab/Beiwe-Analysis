warps = function(points, warp_matrix, pushes, sd1=1, sd2=1, seed = NA){
  # input:  points: 2xN matrix of two-dimensional points   
  #         warp_matrix: 2xK matrix of two-dimensional locations that warp `points`.
  #         pushes: vector of integers, 1 iff warp_matrix row value pushes points.  (pulls otherwise)
  #         sd1: breadth of the warp in dimension 1
  #         sd2: breadth of the warp in dimension 2
  #         seed: optional.  Possible setting seed for reversability.
  # output: 2xN matrix of warped two-dimensional points  
  if(!is.na(seed)) set.seed(seed) 
  total_diffs = points*0
  if(is.vector(warp_matrix)) warp_matrix = t(warp_matrix)
  for(i in 1:nrow(warp_matrix)){
    warp = warp_matrix[i,]
    diffs = t(t(points)-warp)
    total_diffs = total_diffs + diffs * dnorm(diffs[,1]-warp[1],sd=sd1)*dnorm(diffs[,2]-warp[2],sd=sd2) * 2*pi*sd1*sd2 * ifelse(as.logical(pushes[i]),1,-1)
  }
  return(points+total_diffs)
}


# #### Example
# 
# num_warps  = 5
# 
# R = 200
# x = seq(0,2*pi, length.out=R)
# points = cbind(cos(x),sin(x))
# points = cbind(
#   c(seq(-1,1,length.out=R/4),rep(1,R/4),seq(1,-1,length.out=R/4),rep(-1,R/4)),
#   c(rep(-1,R/4), seq(-1,1,length.out=R/4),rep(1,R/4),seq(1,-1,length.out=R/4))
# )
# 
# warp_matrix = matrix(runif(num_warps*2,-2,2),num_warps,2)
# pushes = rep(c(1,0),length.out=num_warps)
# warped = warps(points, warp_matrix, pushes, sd1=2,sd2=2)
# #scale(warped,center=TRUE,scale=TRUE)
# 
# #example_plot
# scaled = TRUE
# pdf("square_warps_scaled.pdf",width=20,height=20)
# par(mfrow=c(5,5),xpd=NA)
# for(j in 1:25){
#   num_warps = 5
#   warp_matrix = matrix(runif(num_warps*2,-2,2),num_warps,2)
#   pushes = rep(c(1,0),length.out=num_warps)
#   plot(points, col=NA, xlim=c(-2,2),ylim=c(-2,2), xlab="", ylab="",axes=FALSE)
#   for(i in seq(0,3,length.out=5)){
#     warped = warps(points, warp_matrix, pushes, sd1=i,sd2=i)
#     points(scale(warped,center=scaled,scale=scaled), col=rainbow(R, alpha = (1-i/3)**2),pch=16)
#   }
#   lines(scale(points,center=scaled,scale=scaled),type="l", col=rgb(.6,.6,.6),lwd=2)
#   if(scaled)lines(scale(warped,center=scaled,scale=scaled), col=rgb(.3,.3,.3),lwd=2)
#   points(warp_matrix,pch=18,cex=3, col=ifelse(pushes,"red", "blue"))
# }
# dev.off()


