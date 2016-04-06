library(SparseGrid)

sg.int<-function(fun,lower,upper, dim){
  ## package SparseGrid is used in the function, so it is required
require("SparseGrid")

 ## taking the lowest value of the lower argument
 lower<-floor(lower)
 
 ## taking the highest value of the higher argument
 upper<-ceiling(upper)
 
 ## the upper limit must be larger than the lower
 if (any(lower>upper)) stop("lower must be smaller than upper")
 
 ## creating a function to use to make the matrix
 grid<-function(x){
   seq(from = lower[x], to = upper[x]-1, by=1)
 }
 
 ?seq
 
 ## making the matrix
 gridss<-as.matrix(expand.grid(lapply(1:dim, grid))) 
 
 ## creating an integration grid to use for integration
 sp.grid <- createIntegrationGrid( 'KPU', dimension=dim, k=5 )

 nodes<-gridss[1,]+sp.grid$nodes

 weights<-sp.grid$weights

 for (i in 2:nrow(gridss))

 {
    nodes<-rbind(nodes,gridss[i,]+sp.grid$nodes)  

    weights<-c(weights,sp.grid$weights)

  }

  gx.sp <- apply(nodes, 1, fun,...)
  val.sp <- gx.sp %*%weights
  val.sp
}

fun_example<-function(x) x^2


sg.int(fun = fun_example, lower = c(4,5,6,7), upper = c(4,5,6,7), dim = 2)
?nodes


