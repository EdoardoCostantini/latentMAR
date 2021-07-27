# Project:   latentMAR
# Objective: function to generate data according to a CFA model
# Author:    Edoardo Costantini
# Created:   2021-07-27
# Modified:  2021-07-27

dataGen <- function(parms, cond){

# Example Input -----------------------------------------------------------

  # cond    <-  conds[2, ]

# Latent Variables Covariance matrix --------------------------------------

  Phi <- matrix(parms$lv_cov, parms$L, parms$L)
  diag(Phi) <- parms$lv_var

# Factor loadings (random factor) -----------------------------------------

  lambda <- rep(cond$lambda, parms$P)

# Observed Items Error Covariance matrix ----------------------------------
# Note: you are creating uncorrelated errors for the observed items

  Theta <- diag(parms$P)
  for (i in 1:length(lambda)) {
    Theta[i, i] <- 1 - lambda[i]^2 * Phi[1, 1]
  }

# Items Factor Complexity = 1 (simple measurement structure) --------------
# Reference: Bollen1989 p234

  Lambda <- matrix(nrow = parms$P, ncol = parms$L)
  start <- 1
  for (j in 1:parms$L) {
    end <- (start + parms$J) - 1
    vec <- rep(0, parms$P)
    vec[start:end] <- lambda[start:end]
    Lambda[, j] <- vec
    start <- end + 1
  }

# Sample Scores -----------------------------------------------------------

  scs_lv    <- mvrnorm(parms$N, rep(parms$lv_mean, parms$L), Phi)
  scs_delta <- mvrnorm(parms$N, rep(0, parms$P), Theta)

# Compute Observed Scores -------------------------------------------------

  x <- data.frame(matrix(nrow = parms$N, ncol = parms$P))
  for(i in 1:parms$N){
    x[i, ] <- t(Lambda %*% scs_lv[i, ] + scs_delta[i, ])
  }

# Rescale Observed Scores -------------------------------------------------

  x_scaled <- sapply(x, function(x) x*sqrt(parms$item_var))
  x_center <- x_scaled + parms$item_mean
  x <- as.data.frame(x_center)

# Give meaningful names ---------------------------------------------------

  colnames(x) <- paste0("z", 1:ncol(x))
  colnames(scs_lv) <- paste0("lv", 1:ncol(scs_lv))

# Return Output -----------------------------------------------------------

  return(
    list(dat_ob = as.data.frame(x),
         dat_lv = as.data.frame(scs_lv),
         Phi    = Phi,
         Theta  = Theta,
         Lambda = Lambda)
  )
}
