"opttodes" <-
function( trt,b,k,iter=50 ){

 require(Dopt)

 tr<-factor(1:trt)
 bl<-c(rep(1:b, rep(k,b))) 
 i<-0                                             # initialise stop criterions i, BIB
 BIB<-FALSE
 while( (!BIB) && (i<iter) ){
   i <- i+1
   d <-Dopt(~tr,bl)
   trtsequence <- d[,trt+1]
   des <- matrix(trtsequence,b,k,byrow=TRUE)
   BIB <- all(isGYD(des,TRUE,FALSE,TRUE)[1:4])
 }
 isGYD(des)
 des
}
