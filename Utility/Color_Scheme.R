cols = function(n,dirtiness=.2,darkness=.8,transparency=0,...){
  alpha = 1 - transparency
  colors = rainbow(n,alpha,...) 
  f = function(colors, val) strtoi(paste("0x",substr(colors,2*val, 2*val + 1),sep="")) 
  rgb = cbind(f(colors, 1),f(colors, 2),f(colors, 3))
  new_rgba = cbind(floor((rgb+(1-rgb/256)*dirtiness*256)*(1-darkness*dirtiness)),f(colors, 4))
  new_colors = apply(matrix(format(as.hexmode(new_rgba),upper.case=TRUE,width=2),ncol=4),1,function(strings) paste("#",paste(strings,collapse=""),sep=""))
  return(new_colors)
}

light_color = function(color, ink_depth, ink_minimum = .2){
  if(is.na(ink_depth) || is.null(ink_depth)){ink_depth=0}else{  ink_depth = ink_depth*.8+.2}
  output = rgb(1-(1-color[1])*ink_depth, 1-(1-color[2])*ink_depth, 1-(1-color[3])*ink_depth) 
  return(output)
}

cerulean    = c(.2,.3,.8)
vermillion  = c(.8,.25,.2)
limegreen   = c(.35,1,0)

sleep_col = rgb(.2,.2,.8)
wake_col  = rgb(.8,.2,.2)

