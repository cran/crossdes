"rand.design.RC" <-
function( design, dat, tau1, rho, n, where ) { 

  require(MASS)

  # Preliminary operations
                         

  if(!is.matrix(design) || !is.numeric(design)){stop("Please check your design matrix")}
  trt <- max(design)
  if(any( sort(unique(as.vector(design))) != 1:length(unique(as.vector(design))))){stop("Please check your design matrix")}
  b   <- nrow(design)
  k   <- ncol(design)
  if(any( c(trt,b,k) == 1)){stop("Please check your design matrix")}
  if(length(design)!=length(dat)){stop("Length of data doesn't match number of plots")}
 
  Qt  <- Q.t(trt)                                # I_t - 1/t 1_t 1_t'                  
  kron<- kronecker( Q.t(b), Q.t (k) )            # Corrects for row and column effects 
  
  fg  <- (b-1)*(k-1)-trt+1                       # Model degrees of freedom

  tauhat    <- array(0,dim=c(n,trt,6))           # simulated tâu_1 , ..., tâu_t             
  varhat    <- matrix(0,ncol=6,nrow=n)           # simulated estimates of var(tau_1 - tau_t)

 
# Main Loop

  for (i in 1:n){
 
    d   <- random.bailey(design)                 # randomised design (rows and tratment labels)
    dpr <- t(d)
    Tdd <- Td(d)                                 # treatment design matrix 
    Tddpr <- t(Tdd)
    
    cma <- round( Tddpr%*%kron%*%Tdd, 14)        # information matrix, rounded to 14 digits
    gcma <- ginv(cma)                            # Moore-Penrose generalized inverse of the information matrix
    lgcmal <- gcma[1,1] - 2*gcma[1,trt] + gcma[trt,trt]
                                                 # We are interested in the contrast l'tau= tau_1 - tau_t.
                                                 # lgcmal is l'C^+ l= c+_11 - 2c+_1t + c+_tt.
                                                  
    gcmaTddprkron <- gcma %*% Tddpr %*% kron 
    
    g <- which(dpr==1)                           # Plots with treatment 1               
    w <- which(dpr==1)[which(dpr==1) %% k != 0]  # Plots with treatment 1 (unless last plot in row) 
    a <- which(dpr==2)[which(dpr==2) %% k != 0]  # Plots with treatment 2 (unless last plot in row) 
    
                                          
    # Visit the different combiations of treatment effect and neighbour effect we are interested
      
    # non main effects, no residual effects
    
    tauhat[i,,1] <- gcmaTddprkron %*% dat
    varhat[i,1] <- ( t(dat)%*%kron%*%dat - t(tauhat[i,,1])%*%cma%*%tauhat[i,,1] ) 
                                                 # sigma2hat * fg
                                                                                                              
    # tau_1 = tau1, all other tau_i = 0 and rho = 0
    
    dat.e <- dat
    dat.e[g] <- dat[g] +tau1 
    
    tauhat[i,,2] <- gcmaTddprkron %*% dat.e   
    varhat[i,2] <- ( t(dat.e)%*%kron%*%dat.e - t(tauhat[i,,2])%*%cma%*%tauhat[i,,2] ) 
    
    # tau = 0, rho_2 = rho, all other rho_i = 0
    
    dat.e <- dat
    dat.e[a+1] <- dat[a+1] +rho 
    
    tauhat[i,,3] <- gcmaTddprkron %*% dat.e 
    varhat[i,3] <- ( t(dat.e)%*%kron%*%dat.e - t(tauhat[i,,3])%*%cma%*%tauhat[i,,3] ) 
      
    # tau_1 = tau1, rho_2 = rho and all other tau_i, rho_i = 0
    
    dat.e <- dat
    dat.e[g] <- dat[g] +tau1 
    dat.e[a+1] <- dat.e[a+1] +rho 
    
    tauhat[i,,4] <- gcmaTddprkron %*% dat.e
    varhat[i,4] <- ( t(dat.e)%*%kron%*%dat.e - t(tauhat[i,,4])%*%cma%*%tauhat[i,,4] ) 
    
    # tau = 0, rho_1 = rho, all other rho_i = 0
    
    dat.e <- dat 
    dat.e[w+1] <- dat[w+1] +rho 
    
    tauhat[i,,5] <- gcmaTddprkron %*% dat.e
    varhat[i,5] <- ( t(dat.e)%*%kron%*%dat.e - t(tauhat[i,,5])%*%cma%*%tauhat[i,,5] )
    
    # tau_1 = tau1, rho_1 = rho and all other tau_i, rho_i = 0
    
    dat.e <- dat
    dat.e[g] <- dat[g] +tau1 
    dat.e[w+1] <- dat.e[w+1] +rho 
    
    tauhat[i,,6] <- gcmaTddprkron %*% dat.e
    varhat[i,6] <- ( t(dat.e)%*%kron%*%dat.e - t(tauhat[i,,6])%*%cma%*%tauhat[i,,6] ) 
    
    varhat[i,] <- varhat[i,] * lgcmal
  }
  
  varhat <- varhat / fg 
                                                 # sigma2hat * l'C^+l            
  
  
  # Tidying up the results, storing results
  
  contrast <- tauhat[,1,] - tauhat[,trt,]               
                                                 # Estimates of l'tau = tau_1 -tau_t

  mod <- (6*n)%%5                                # The file is to have 5 entries per row, may need to fill up with NAs
  if(mod){ contrast<-c( contrast,rep(NA,5-mod) ) }    
  if(mod){ varhat<-c( varhat,rep(NA,5-mod) ) } 

  write( as.vector(contrast), file=where )
  write( as.vector(varhat),  file=where, append=TRUE )
  write( c(n, trt, b, k, fg, tau1, rho, rep(NA,3)), file=where, append=TRUE ) 
                                                 # Attach information about the design
      
}
