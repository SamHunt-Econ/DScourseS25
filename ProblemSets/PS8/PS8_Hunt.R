#installing packages
install.packages("nloptr")
library(nloptr)

#Creating the dataset
set.seed(100)
N <- 100000
K <- 10

X <- matrix(rnorm(N * (K - 1)), nrow = N, ncol = (K - 1))
X <- cbind(1, X)

eps <- rnorm(N, mean = 0, sd = 0.5)
beta <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)
Y <- X%*%beta + eps

##computing beta_ols using the closed form solution
beta_ols <- solve(t(X)%*%X)%*%t(X)%*%Y
beta_ols

##Calculating beta_ols using gradient descent
alpha <- 0.0000003
max_iter <- 100
objfun <- function(beta, Y, X){
  return(sum((Y - X%*%beta)^2))
}

gradient <- function(beta, y_i, X_i) {
  # For a single observation: -2 * X_i^T * (y_i - X_i*beta)
  return(-2 * t(X_i) * (y_i - sum(X_i * beta)))
}

#storage for beta values
beta_history <- matrix(0, nrow=ncol(X), ncol=max_iter)
beta_history[,1] <- beta

# stochastic gradient descent method
iter <- 1
converged <- FALSE
beta_prev <- beta + 1  # Ensure different from beta initially

while (!converged && iter < max_iter) {
  # Store previous beta
  beta_prev <- beta
  
  # Randomly re-order the data
  random <- sample(nrow(X))
  X_shuffled <- X[random,]
  y_shuffled <- Y[random]
  
  # Update parameters for each row of data
  for(i in 1:nrow(X)) {
    X_i <- as.numeric(X_shuffled[i,])  # Convert to numeric vector
    y_i <- y_shuffled[i]
    grad <- gradient(beta, y_i, X_i)
    beta <- beta - alpha * grad
  }
  
  # Reduce learning rate
  alpha <- alpha / 1.0005
  
  # Store current beta
  if (iter < max_iter) {
    beta_history[, iter+1] <- beta
  }
  
  # Check convergence
  if (sqrt(sum((beta - beta_prev)^2)) < 1e-12) {
    converged <- TRUE
  }
  
  # Print progress
  if (iter %% 1000 == 0) {
    cat("Iteration:", iter, "Beta:", beta, "\n")
  }
  
  iter <- iter + 1
}

#view results
cat("Total iterations:", iter - 1, "\n")
cat("Final beta values:", beta, "\n")

#resetting beta
beta <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2) 

##Computing beta_ols using nloptr L-BFGS
objfun <- function(beta,Y,X) {
  return (sum((Y-X%*%beta)^2))
}

gradient <- function(beta,Y,X) {
  return ( as.vector(-2*t(X)%*%(Y-X%*%beta)) )
}

options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e3)
beta0 <- runif(dim(X)[2])
result_LBFGS <- nloptr( x0=beta0,eval_f=objfun,eval_grad_f=gradient,opts=options,Y=Y,X=X)
print(result_LBFGS)


##Computing beta_ols using Nelder-Mead
options <- list("algorithm" = "NLOPT_LN_NELDERMEAD","xtol_rel" = 1.0e-6,"maxeval" = 1e3)
beta0 <- runif(dim(X)[2])
result_NM <- nloptr(x0=beta0,eval_f=objfun,opts=options,Y=Y,X=X)
print(result_NM)

#Our answers do differ. There are both quite accurate but the estimates are different.

##Computing beta_MLE using nloptr L-BFGS
objfun  <- function(theta,Y,X) {
  beta    <- theta[1:(length(theta)-1)]
  sig     <- theta[length(theta)]
  loglike <- -sum( -.5*(log(2*pi*(sig^2)) + ((Y-X%*%beta)/sig)^2) ) 
  return (loglike)
}
gradient <- function (theta,Y,X) {
  grad     <- as.vector(rep(0,length(theta)))
  beta     <- theta [1:(length(theta)-1)]
  sig      <- theta [length(theta)]
  grad[1:(length(theta)-1)] <- -t(X)%*%(Y - X%*%beta)/(sig^2)
  grad[length(theta)]       <- dim(X)[1]/sig-crossprod (Y-X%*%beta)/(sig^3)
  return ( grad )
}

theta0 <- runif(dim(X)[2]+1)
options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e4)
result <- nloptr( x0=theta0,eval_f=objfun,eval_grad_f=gradient,opts=options,Y=Y,X=X)
print(result)
betahat  <- result$solution[1:(length(result$solution)-1)]
sigmahat <- result$solution[length(result$solution)]
print(betahat)
print(sigmahat)

##Computing beta_ols using linear regression
library(modelsummary)
modelsummary(lm(Y~X-1), output = "latex")