"analyze.rand" <-
function(where, fig=FALSE, ref=FALSE, refval=numeric(6), reftext="Reference Value", 
 pch1=46, col1="red", col2="black",  ...){

  if(!(ref %in% c("azais","bailey","contrast","other"))){
   "Please choose a different reference value for the mean of the estimated contrast"}

  a   <- scan(file=where)                        # Scan the file.
  la  <- length(a)                               # 12*n + 10 or 12*n + 20, depending on whether n%%5 equals 0 
  
  n   <- a[la-9]                                 # No of simulation runs   
  
  if(n%%100){stop("n is not a multiple of 100!")}
  n1 <- n/100                                    # The data from the simulation study is partitioned
                                                 # into n1 groups of 100 observations each.
  
  trt <- a[la-8]                                 # No of treatments        
  b   <- a[la-7]                                 # No of rows              
  k   <- a[la-6]                                 # No of columns           
  fg  <- a[la-5]                                 # Model degrees of freedom    
  tau1<- a[la-4]                                 # Value for tau_1             
  rho <- a[la-3]                                 # Value for rho_1 resp. rho_2 

  if(!ref){refval <- c(0,tau1,0,tau1,0,tau1)}    # true value of the contrast l'tau (otherwise use refval from above)
    
                                                                                                                               
  contrast <- numeric(n)
  varhat <- numeric(n)
  tstat   <- numeric(n)                          # Permutation t-statistics
  Z <- 0                                         # Statistic to test if contrast estimate is unbiased
                            
  s2teilcontrast <- matrix(0,6,n1)
  
  meanteilvar <- matrix(0,6,n1)                  # Average value of the variances in the subgroups
  vardiff <- numeric(n1)                         # Estimate of the difference var(l'tâu)-E(vâr(l'tâu)) 
  kivardiff <- numeric(3) 
   
  out1 <- matrix(0,nrow=6,ncol=5)
  rownames(out1) <- 1:6
  colnames(out1) <- c("      No < qt", "  Level/Power", "   ave. l'tâu", "   compare to", "            Z")

  out2 <- matrix(0,nrow=6,ncol=5)
  rownames(out2) <- 1:6
  colnames(out2) <- c("  s^2 (l'tâu)", "     ave. vâr", "  lower bound", "mean(var-vâr)", "  upper bound")     
   
   
  # Rearrange the data in the file. There are n entries for tâu_1 - tâu_t in Case 1, then there are 
  # n entries for tâu_1 - tâu_t in Case 2 etc. The contrast estimates are followed by the varhat-values.
      
  for (i in 6:1){
    contrast <- a[ ((i-1)*n + 1) : ((i-1)*n + n) ] 
    varhat <- a[  (.5*(la-10) + (i-1)*n + 1) : (.5*(la-10) + (i-1)*n + n) ]
    tstat <- contrast / sqrt( varhat )           # Randomization t-statistics
    Z <- sqrt(n)*( mean(contrast)-refval[i] )/sqrt(var(contrast))                
                   # is approx. standard normal, i.e. absolute value larger than 1.96 means signif. bias of l'tâu
                                       
    for (j in 1:n1){
      s2teilcontrast[i,j] <- var(contrast[((j-1)*100+1):((j-1)*100+100)])
                                                 # Partition the permutations in n1 groups of 100 observations each and
                                                 # compute the empirical variance of the contrast estimates in each set.
                                    
      meanteilvar[i,j] <- mean(varhat[((j-1)*100+1):((j-1)*100+100)])   
                                                 # same for the averages of vâr(l'tâu) in each subset
    } 
    
    vardiff <- (s2teilcontrast[i,]-meanteilvar[i,])  
   
    kivardiff <- c( mean(vardiff) - qt(.975,n1-1)*sqrt(var(vardiff)/n1), mean(vardiff),
      mean(vardiff) + qt(.975,n1-1)*sqrt(var(vardiff)/n1) )  
                                                 # confidence interval for var-E(vâr) of the n1 subgroups
                                                 # 1st column lower end, 2nd column estimate of var-E(vâr)
                                                 # 3rd column upper end.
    
 # Tables of results
   
    out1[i,] <- c( sum(tstat< qt(.05,fg)) , round(sum(tstat< qt(.05,fg))/n,4) , 
     round(mean(contrast),4), round(refval[i],4), round(Z,2) )
    out2[i,] <- c( round(var(contrast),4) , round(mean(varhat),4), round(kivardiff,4) ) 
                                                
 # Various Graphs
 
    if (fig) {
      
    
      x11()
      qqnorm(vardiff,main=paste("Normal Q-Q Plot of var - vâr, Case",i), ...) # q-q-plot of the difference of variance estimates
      x11()
      qqnorm(contrast, main =paste("Normal Q-Q Plot of tau_1 - tau_5, Case",i),
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", pch=pch1, ...)
                                                 # q-q-plot of the contrasts for all 6 cases 
                                                   
      x11()
      tcdf.plot(tstat,n,fg,paste("Empirical cumulative distribution function","\n","of the t-statistic, case",i),
       "x", 2, col1=col1, col2=col2, ...)
                                                 # Comparison of the empirical CDF of the t-statistics
                                                 # with the CDF of the t distribution
                
      x11()
      minmax <- sort(contrast)[c(1,n)]
      histogr <- invisible(  hist( contrast,sqrt(n)  )  ) 
      histogr <- hist( contrast,sqrt(n),main=paste("Histogram of the contrast, case",i) ,xlab="x",ylab="count", 
       xlim=c(min(c(minmax[1],refval[i])),max(c(minmax[2],refval[i]))), ylim=c(0,1.1*max(histogr$co)), ... )          
                                                 # Histogram of the contrast estimates
      abline(v=refval[i],col=col1)
      legend( min(contrast),1.1*max(histogr$co), reftext ,lty=1,col=col1)
                                                 # At least for GYD (???)
    }
  }
  
   

  out <- list(out1,out2)
  names(out) <- c("Table 1", "Table 2")

  cat("\n")
  print(out)
  cat("\n")                                 
  
}
