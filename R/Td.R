"Td" <-
function( d ){

  trt <- max(d)
  b   <- nrow(d)
  k   <- ncol(d)
  Td <- matrix(rep(0,trt*b*k),ncol=trt)
  dtrt<-diag(trt)

  for (i in 1:b){
    for (j in 1:k){
      Td[(i-1)*k+j,] <- dtrt[d[i,j],]
      }}
  Td
}
