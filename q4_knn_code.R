## STAT318/462 kNN regression function

kNN <- function(k,x.train,y.train,x.pred) {

  ## Initialize:
  n.pred <- length(x.pred);		
  y.pred <- numeric(n.pred)
  
  ## Main Loop
  for (i in 1:n.pred){
    d <- abs(x.train - x.pred[i])
    dstar = d[order(d)[k]]
    y.pred[i] <- mean(y.train[d <= dstar])		
  }
  ## Return the vector of predictions
  invisible(y.pred)
}

## Load data in as x.train, y.train 
## Use it to create a x.pred vector 
## Find out which value is the MSE 
## Run kNN function for each k and produce the MSE 
## Plot the kNN function with the lowest MSE 