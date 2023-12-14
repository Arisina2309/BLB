#' Function for BLB in quantile regression
#'
#' Implements the BLB algorithm in quantile regression
#'
#' @param X numeric matrix of covariates
#' @param Y numeric vector of responses
#' @param b numeric input for the subset size
#' @param s numeric input for the number of sampled subsets
#' @param r numeric input for the number of Monte Carlo iterations
#' @param q numeric input for the quantile for quantile regression
#'
#' @keywords quantile regression
#'
#' @return blb estimate of coefficients and sd of the estimates
#'
#' @export

blb_quantile_regression <- function(X, Y, b, s, r, q = 0.5){
  require(quantreg)
  n = nrow(X)
  p = ncol(X)
  if(n != length(Y)){
    stop("Incompatible dimensions for X and Y")
  }
  if(b > n){
    stop("Subset size 'b' is greater than n")
  }

  # Subsample the data
  Xi_sd = Xi_mean = matrix(NA, nrow = s, ncol = p)

  for(j in 1:s){
    subsample = sample(1:n, b, replace = FALSE)
    Xsub = X[subsample,]
    Ysub = Y[subsample]

    fit = matrix(NA, nrow = r, ncol = p)

    # Approximate assessment of estimator
    for(k in 1:r){
      multisamp = sample(1:b, n, replace = TRUE)
      Xboot = Xsub[multisamp,]
      Yboot = Ysub[multisamp]
      fit[k,] = as.numeric(coef(rq(Yboot ~ 0 + Xboot, tau = q)))
    }

    Xi_sd[j,] = apply(fit, 2, sd)
    Xi_mean[j,] = colMeans(fit)

  }
  return(list(sd = colMeans(Xi_sd), means = colMeans(Xi_mean)))
}
