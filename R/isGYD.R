"isGYD" <-
function(d, invis=FALSE, tables=FALSE, coded=FALSE){

  rows <- design.row(d)                       # Analyze d w.r.t. rows
  cols <- design.row(t(d))                    # Analyze d w.r.t. columns 
  rows4 <- rows[[4]]
  cols4 <- cols[[4]]
  dummy<-TRUE

  if (!invis){
    
    cat("\n")
    if( all(c(rows4[c(1:3,5)],cols4[c(1:3,5)])) )
      { print("The design is a latin square.",quote=FALSE) }
    else
      { if( all(c(rows4[c(1:3,6)],cols4[c(1:3,6)])) )
          { print("The design is a generalized latin square.",quote=FALSE) } 
        else
          { if (all(c(rows4[c(1:3,6)],cols4[1:3])) )
              { print("The design is a regular generalized Youden design that is uniform on the rows.",quote=FALSE) }     
            else
              { if (all(c(rows4[1:3],cols4[c(1:3,6)])) ) 
                  { print("The design is a regular generalized Youden design that is uniform on the columns.",quote=FALSE) }
                else
                  { if (all(c(rows4[1:3],cols4[1:3])) ) 
                      { print("The design is a generalized Youden design.",quote=FALSE) }
                    else 
                      { dummy <- FALSE }         # Check for various types of generalized Youden designs       
                  }
              }
          }
      }
  
    if( !dummy )
      { if (all(rows4[c(1:3,5)]))
          { print("The design is a balanced complete block design w.r.t. rows.",quote=FALSE) }
        else 
          { if (all(rows4[c(1:3,4)]))
              { print("The design is a balanced incomplete block design w.r.t. rows.",quote=FALSE) }
            else
              { if (all(rows4[c(1:3)]))
                  { print("The design is a balanced block design w.r.t. rows.",quote=FALSE) }
                else 
                  { if (all(cols4[c(1:3,5)]))
                      { print("The design is a balanced complete block design w.r.t. columns.",quote=FALSE) }
                    else
                      { if (all(cols4[c(1:3,4)]))
                          { print("The design is a balanced incomplete block design w.r.t. columns.",quote=FALSE) }
                        else 
                          { if (all(cols4[c(1:3)]))
                              { print("The design is a balanced block design w.r.t. columns.",quote=FALSE) }
                            else
                              { print("The design is neither balanced w.r.t. rows nor w.r.t. columns.",quote=FALSE) }
                          }                      # Check for various types of balanced designs  
                      }
                  }
              }
          }
      }    
  
  cat("\n")     
  }
  
  # Tables and characteristica of the design

  out <- list(rows[[1]],rows[[2]],cols[[2]],rows[[3]],cols[[3]])
  names(out) <- c("Number of occurences of treatments in d", "Row incidence matrix of d", "Column incidence matrix of d",
   "Rows with occurences of pairs", "Columns with occurences of pairs" )  

  typeout <- c(rows4,cols4) 

  if(tables){ print(out) }
  if(coded){ typeout }

}
