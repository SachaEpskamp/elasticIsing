elasticIsing <- function(
  data, # Binary data
  lambda = exp(seq(log(1), log(0.001), length = 100)), # Vector of penalty lambdas to test (NEEDS TO BE IMPROVED!!!)
  alpha = seq(0,1,length=100), # Vector of alphas to test.
  cost = c('mspe','rmspe','mape','tmspe','rtmspe'), # Cost functions to apply
  K = 10, # number of folds
  and = TRUE
){
  data <- as.matrix(data)
  
  # Parameters:
  Np <- nrow(data) # Number of persons
  Ni <- ncol(data) # Number of items
  Nl <- length(lambda) # Number of lambdas
  Na <- length(alpha) # Number of alphas
  Nc <- length(cost) # Number of cost functions
  
  # Make lambda decreasing:
  lambda <- sort(lambda, decreasing = TRUE)
  
  # Create folds:
  Folds <- cvTools::cvFolds(Np, K)
  
  # Test if data is binary, stop if not:
  if (!all(sort(unique(c(unlist(data)))) == c(0,1))){
    stop("Binary data required")
  }
  
  # Matrix to store prediction cost:
  predictionCost <- array(NA, c(Nl, Na,Nc))
  dimnames(predictionCost) <- list(lambda,alpha,cost)
  
  # For every alpha:
  for (a in seq_len(Na)){
    
    # List with predicted values for every lambda:
    PredictedValues <- rep(list(matrix(NA,Np, Ni)), Nl)
    
    # For every fold:
    for (k in seq_len(K)){
      # Which in block:
      inBlock <- sort(Folds$subsets[Folds$which==k,1])
      
      # Estimte Ising:
      foldEstimates <- elasticIsingInner(data[-inBlock,], lambda, alpha[a],   and)
      
      # For every lambda, predict and store:
      for (l in seq_len(Nl)){
        PredictedValues[[l]][inBlock,] <- predictIsing(foldEstimates$networks[[l]], foldEstimates$thresholds[[l]], data[inBlock,])
      }
    }
    
    # Compute prediction cost:
    for (c in seq_len(Nc)){
      predictionCost[,a,c] <- sapply(PredictedValues, function(p) do.call(cost[c],list(data, p))) 
    }
  }
  
  # Select values with minimal predictive cost:
  Mininals <- list()
  for (c in seq_len(Nc)){
    Mininals[[c]] <- which(predictionCost[,,c] == min(predictionCost[,,c]), arr.ind = TRUE)
    Mininals[[c]] <- as.data.frame(Mininals[[c]])
    Mininals[[c]][,1] <- lambda[Mininals[[c]][,1]]
    Mininals[[c]][,2] <- alpha[Mininals[[c]][,2]]
    colnames(Mininals[[c]]) <- c("lambda","alpha")
    rownames(Mininals[[c]]) <- NULL
  }
  
  #   
  #   cat("Minimal prediction costs found at:\n")
  #   print(Minimal)
  #   
  Res <- list(
    minimal = Mininals,
    costs = predictionCost,
    lambda = lambda,
    alpha = alpha,
    data = data,
    and = and)
  class(Res) <- "elasticIsing"
  return(Res)
}


## Inner function to compute elastic net Ising model for series lambda and single alpha:
elasticIsingInner <- function(
  data, # Binary data
  lambda, # Vector of penalty lambdas to test
  alpha, # Vector of alphas to test.
  and = TRUE
){
  data <- as.matrix(data)
  # Parameters:
  Np <- nrow(data)
  Ni <- ncol(data)
  Nl <- length(lambda)
  stopifnot(length(alpha)==1)
  
  # Run glm for every node:
  ResNodes <- list()
  
  for (n in seq_len(Ni)){
    ResNodes[[n]] <- glmnet(as.matrix(data[,-n]),data[,n],"binomial",alpha = alpha, lambda = lambda)
  }
  
  # Construct networks for every lambda:
  Coefs <- lapply(ResNodes, coef)
  Nets <- rep(list(matrix(0,Ni,Ni)),Nl)
  Thresh <- rep(list(vector("numeric", Ni)), Nl)
  
  for (i in seq_len(Ni)){
    for (l in seq_len(Nl)){
      Nets[[l]][-i,i] <- Coefs[[i]][-1,l]
      Thresh[[l]][i] <- Coefs[[i]][1,l]
    }
  }
  
  # Symmetrize:
  if (and){
    Nets <- lapply(Nets, function(x) (x!=0 & t(x)!= 0) * (x + t(x))/2  )
  } else {
    Nets <- lapply(Nets, function(x) (x!=0 | t(x)!= 0) * (x + t(x))/2  )
  }
  
  # Return:
  return(list(networks = Nets, thresholds = Thresh))
}


