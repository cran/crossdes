"des.MOLS" <-
function(trt,k){
x <- MOLS(trt[1],trt[2]) 
d <- NULL
for( i in 1:(trt[1]^trt[2] -1) ){ d <- rbind(d, t(x[,,i])) }
d <- d[,1:k] 
}
