"design.row" <-
function(d){

  require(combinat)

  if(!is.matrix(d) || !is.numeric(d)){stop("Please check your design matrix")}
  trt <- max(d)
  if(any( sort(unique(as.vector(d))) != 1:length(unique(as.vector(d))))){stop("Please check your design matrix")}
  b   <- nrow(d)
  k   <- ncol(d)
  if(any( c(trt,b,k) == 1)){stop("Please check your design matrix")}
  a1  <- floor(k/trt)+1

  td    <- Td(d)                                 # Treatment design matrix
  bd    <- kronecker(diag(b),rep(1,k))           # Block design matrix
  occ   <- diag(t(td)%*%td)                      # Number of occurences of treatments in design d
  rinc  <- t(bd)%*%td                            # Row-incidence matrix

if( (min(rinc) < a1-1) | (max(rinc) > a1) )
  { bin.row<-FALSE }
else
  { bin.row<-TRUE }                              # TRUE if the design is binary w.r.t rows
    
  pairwise<-rinc[,combn(trt,2)]        
  sumpair<-matrix(0,b,trt*(trt-1)/2)
  for (i in 1:(trt*(trt-1)/2)){
    for (j in 1:b){                       
      if( all(pairwise[j,(2*i-1):(2*i)]) )       # either >0 or =0
        { sumpair[j,i]<-1}
      else
        { sumpair[j,i]<-0}
  }}
      
  sumpair<-apply(sumpair,2,sum)                  # Get no. of blocks with treatments i,j, i != j.

  blockocc <- ifelse( rinc>0, 1, 0)
  blockocc <- apply(blockocc,2,sum)              # Get no. of blocks with occurences of treatment i.

  pairinc <- matrix(0,trt,trt)
  pairinc[which(lower.tri(pairinc))] <- sumpair
  pairinc <- pairinc + t(pairinc)
  diag(pairinc) <- blockocc
                                                 # pairinc gives number of blocks, where treatments i and j both appear.
                                                 # For a binary design with k<=t this is the number of times that i and j 
                                                 # appear together in a block, summed over all blocks. 
  pair.row <- matrix(0,nrow=trt,ncol=trt)
  if(bin.row){
    for (i in 1:trt) {
       for (j in i:trt){
         pair.row[i,j] <- sum( (rinc[,i]+rinc[,j])==(2*a1) )
  }}}
                                                 # For each pair i,j of treatments the number of rows
                                                 # satisfying rinc[i,r]=rinc[j,r]=a1 is computed. 
                                                 # Note that this is only checked for binary designs.
                                                 # The formula above works for binary designs since max(rinc)=a1
                                                 # for such designs.

  xi <-  ( b*(k-trt*(a1-1))*(k-trt*(a1-1)-1) )/( trt*(trt-1) )

  # Check type of design 
  
  type <- c(FALSE, bin.row, FALSE, FALSE, FALSE, FALSE)
  if( all(occ==(b*k/trt)) ){ type[1]<-TRUE }     # TRUE if all treatments occur equally often in d
  if( (bin.row==TRUE) && ( all( (pair.row[upper.tri(pair.row)]) == xi ) ) ){ type[3]<-TRUE }   
                                                 # TRUE if in a binary design for each pair i,j of treatments the 
                                                 # number of rows satisfying rinc[i,r]=rinc[j,r]=a1 is equal
                                            
  if(k<trt){ type[4]<-TRUE }                     # TRUE, if d is incomplete w.r.t. rows
  if(k==trt){ type[5]<-TRUE }                    # TRUE, if d is complete w.r.t. rows
  if(((k/trt)%%1)==0){ type[6]<- TRUE }          # TRUE, if a BBD d is uniform on the rows

  # Output
  
  names(occ)     <- 1:trt
  rownames(rinc) <- 1:b
  colnames(rinc) <- 1:trt 
  rownames(pairinc) <- 1:trt
  colnames(pairinc) <- 1:trt
  
  list(occ,rinc,pairinc,type)

}
