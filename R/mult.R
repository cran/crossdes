"mult" <-
function(a,b){


 
  la <- length(a)
  lb <- length(b)
  le <- la+lb-1       # length of result m
  m  <- rep(0,le)     # this will be the result of a*b
  
  dummy <- matrix(0,le,lb)  # to contain partial Results

  for (i in 1:lb){
    dummy[i:(i+la-1), i] <- a*b[i] 
  }
  m <- apply(dummy,1,sum)
  m
}
