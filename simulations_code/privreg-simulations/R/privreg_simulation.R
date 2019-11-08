library(privreg)

# set up the simulation
conditions <- expand.grid(
  outcome = c("gaussian", "binomial"),
  P       = c(10, 50, 100, 200),
  Pcov    = c("low", "high"),
  N       = 1000
)

# data-generating function
gen_dat <- function(outcome = "gaussian", P = 10, Pcov = "low", N = 1000) {
  # Generate raw standard normally distributed data (uncorrelated)
  X_raw <- matrix(rnorm(N*P), N)
  
  # Force a specific covariance matrix on it
  Sigma <- toeplitz(c(1, rep(ifelse(Pcov == "low", 0.1, 0.5), P - 1)))
  X     <- X_raw %*% chol(Sigma)
  
  # Generate a prediction
  beta <- seq(-1, 1, length.out = P) / P * 10
  y_pred <- X %*% beta
  
  # Create the outcome
  eta <- y_pred + rnorm(N, sqrt(tcrossprod(beta %*% Sigma, beta)))
  if (outcome == "gaussian") {
    # R^2 of 0.5
    y <- eta
  } else if (outcome == "binomial") {
    y <- rbinom(N, 1, binomial()$linkinv(eta))
  }
  
  # make the dataset
  list(
    beta  = beta,
    y     = y,
    alice = X[,1:(P/2)],
    bob   = X[,(P/2 + 1):P]
  )
}


# simulation parameters
reps <- 100 # repetitions per condition

library(pbapply)
library(parallel)
cl <- makeCluster(12)
clusterExport(cl, c("gen_dat", "conditions"))
clusterEvalQ(cl, library(privreg))

cndli <- vector("list", nrow(conditions))
for (cnd in 1:nrow(conditions)) {
  cat(cnd, "\n")
  clusterExport(cl, c("cnd"))
  cndli[[cnd]] <- pblapply(1:reps, function(r) {
    fam <- get(as.character(conditions$outcome[cnd]))()
    data <- do.call(gen_dat, conditions[cnd,])
    res <- privreg_local(data$y, data$alice, data$bob, family = fam, maxit = 3e4,
                         debug = FALSE, tol = 1e-11)
    
    # add true stuff
    X <- cbind(data$alice, data$bob)
    eta <- X %*% data$beta
    w <- c(sqrt(fam$mu.eta(eta)^2/fam$variance(fam$linkinv(eta))))
    res$true <- list(coef = data$beta, se = sqrt(diag(solve(crossprod(X * w)))))
    return(res)
  }, cl = cl)
}
stopCluster(cl)


# save results to file
saveRDS(cndli,      file = "./data/results.rds")
saveRDS(conditions, file = "./data/conditions.rds")
