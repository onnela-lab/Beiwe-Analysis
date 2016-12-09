ranges = function(vecs){
  if(!is.matrix(vecs)) vecs = t(as.matrix(as.vector(vecs)))
  return(cbind(vecs[,3]-vecs[,1], vecs[,4]-vecs[,2]))
}

area = function(vecs){
  if(!is.matrix(vecs)) vecs = t(as.matrix(as.vector(vecs)))
  apply(ranges(vecs),1,prod)
}

rect_union = function(vecs){
  if(!is.matrix(vecs)) vecs = t(as.matrix(as.vector(vecs)))
  return(c(min(vecs[,1],na.rm=T), min(vecs[,2],na.rm=T), max(vecs[,3],na.rm=T), max(vecs[,4],na.rm=T)))
}

minimum_relevant_window = function(val, vec){
  if(is.infinite(val)){output = length(vec)}
  else{output = try(min(which(val <= vec)), silent = TRUE)}
  return(output)
}

Find.Group.Memberships = function(Data, S){
  D = nrow(Data)
  areas = apply(Data,1,area)
  R = S[sapply(areas, minimum_relevant_window, vec = S)]
  
  G = rep(NA, D)
  k=1
  G[1] = 1
  for(i in 2:D){
    current_group = union(which(G[1:i] == k), i)
    condition = (R[i] != R[i-1] | area(rect_union(Data[current_group,])) > R[i])
    if(!is.na(condition)){if(condition) k = k + 1}
    G[i] = k
  }
  return(list(G=G, R=R))
}

find.width.padding = function(vec, max_area)
  (-sum(ranges(vec)) + sqrt(sum(ranges(vec))^2+4*(max_area-area(vec))))/2


Width.Padding = function(W, R){
  D = nrow(W)
  ws = sapply(1:D,function(i) find.width.padding(W[i,], R[i]))
  new_W = W + t(t(replicate(4, ws)) * c(-1,-1,1,1)/2)
  return(new_W)
}

Fuzzy.Window = function(Data, S, padding = TRUE){
  memberships = Find.Group.Memberships(Data, S)
  G = memberships$G
  R = memberships$R
  group_windows = t(sapply(1:max(G), function(k){rect_union(Data[which(G == k),])}))
  W = group_windows[G,]
  if(padding) W = Width.Padding(W, R)
  return(list(G=G, W=W))
}

# #### Example
# # Data is a Dx4 matrix with each row containing a day and each column representing xmin, ymin, xmax, and ymax, respectively.
# 
# Data = rbind(
#   c(0,0,.8,.8),
#   c(.2,.2,1,1),
#   c(0,0,10,10),
#   c(0,0,2,2),
#   c(1,1,3,3),
#   c(0,0,1,1),
#   c(0,0,10,10)
# )
# 
# S = c(1,10,100)
# 
# Data
# Fuzzy.Window(Data, S)




