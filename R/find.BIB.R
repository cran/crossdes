"find.BIB" <-
function( trt,b,k,iter=30,check=TRUE ){

 require(AlgDesign)

 i <- 0                             # initialise stop criterions i, BIB
 BIB <- FALSE
 
 while( (!BIB) && (i<iter) ){
   i <- i+1 
   dummy <- optBlock(~.,withinData=factor(1:trt),blocksize=rep(k,b))
   des <- matrix(dummy$rows,byrow=TRUE,nc=k)
   BIB <- all(isGYD(des,TRUE,FALSE,TRUE)[1:4])
 }
 if( check ){ isGYD(des) }
 des
}
 
 
