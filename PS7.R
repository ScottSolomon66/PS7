library(SparseGrid)

sg.int<-function(g,...,lower,upper, dimensions)
  ## package SparseGrid is used in the function, so it is required
{ require("SparseGrid")

 ## taking the lowest value of the lower argument
 lower<-floor(lower)
 
 ## taking the highest value of the higher argument
 upper<-ceiling(upper)
 
 ## the upper limit must be larger than the lower
 if (any(lower>upper)) stop("lower must be smaller than upper")

 ## creating a matrix for the 2 dimensional bounds
 gridss<-as.matrix(expand.grid(seq(lower[1],upper[1]-1,by=1),seq(lower[2],upper[2]-1,by=1)))
 
 sp.grid <- createIntegrationGrid( 'KPU', dimension=dimension, k=5 )

 nodes<-gridss[1,]+sp.grid$nodes

 weights<-sp.grid$weights

 for (i in 2:nrow(gridss))

 {
    nodes<-rbind(nodes,gridss[i,]+sp.grid$nodes)  

    weights<-c(weights,sp.grid$weights)

  }

  gx.sp <- apply(nodes, 1, g,...)
  val.sp <- gx.sp %*%weights
  val.sp
}

sg.int(g = x^2, lower = c(4,5,6,7), upper = c(4,5,6,7))

