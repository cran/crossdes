"all.combin" <-
function(trt,k=trt){
 
  require(gtools)

  permutations(trt,k)
}
