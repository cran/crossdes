"random.RC" <-
function( d ){
  trt <- max(d)
  b   <- nrow(d)
  k   <- ncol(d)
  d <- d[sample(b),]                        # Permutes rows
  a <- sample(trt)                          # Permutes treatment labels (the lables 1:trt change to a)
  list(matrix(a[d],nc=k),a)}
