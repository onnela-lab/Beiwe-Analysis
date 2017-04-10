rgb2col = function(x) strtoi(c(substr(x,2,3),substr(x,4,5),substr(x,6,7)), 16L)/256

cols = function(n,dirtiness=.25, darkness=.2, transparency=0, ...){
  colors = rainbow(n, ...) 
  f = function(colors, val) strtoi(paste("0x",substr(colors,2*val, 2*val + 1),sep="")) 
  rgb = cbind(f(colors, 1),f(colors, 2),f(colors, 3))
  new_rgba = cbind(floor((rgb+(1-rgb/256)*dirtiness*256)*(1-darkness*dirtiness)),round(255*(transparency)))
  if(transparency == 0){
    new_rgba = new_rgba[,1:3]
  }else{
    new_rgba[,4] = round(255*(1-transparency))
  }
  CHARS = format(as.hexmode(new_rgba),upper.case=TRUE,width=2)
  if(n==1){CHARMAT = t(as.matrix(CHARS))}else{CHARMAT = matrix(CHARS,ncol=ncol(new_rgba))}
  new_colors = apply(CHARMAT ,1,function(strings) paste("#",paste(strings,collapse=""),sep=""))
  return(new_colors)
}

lowess_lty = 1
stream_colors = c(cols(1,start=.015), cols(1,start=.07), cols(1,start=.15), cols(1,start=.33,dirtiness=.23,darkness=.7), cols(1,start=.6), cols(1,start=.75,darkness=0),  rgb(.3,.3,.3))

light_color = function(color, ink_depth = .2){
  output = rgb(1-(1-color[1])*ink_depth, 1-(1-color[2])*ink_depth, 1-(1-color[3])*ink_depth) 
  return(output)
}

cerulean    = c(.2,.3,.8)
vermillion  = c(.8,.25,.2)
limegreen   = c(.35,1,0)

dark_line_col = rgb(.3,.3,.3)
light_line_col = rgb(.5,.5,.5)


sleep_col = rgb(.2,.2,.8)
wake_col  = rgb(.8,.2,.2)

light_alpha = 0.7

palette = rev(cols(100,start=0,end=.635,darkness=1))







