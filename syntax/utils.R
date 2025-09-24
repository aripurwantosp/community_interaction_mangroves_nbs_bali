# R file for:
# Utility or helpers function
# 
# For paper:
# 
# Authors of the paper:
# 
# Code by:
# Ari Purwanto Sarwo Prasojo
# 
# Date of this version:
# 2025/09/18

# function for bootstrap cov of OLS or lm object ----
cov_boot <- function(fit, B = 1000, seed = 123) {
  if (!inherits(fit, "lm")) stop("Input must lm object")
  
  if (!is.null(seed)) set.seed(seed)
  
  # Data asli
  data <- model.frame(fit)
  n <- nrow(data)
  X <- model.matrix(fit)
  coef_names <- names(coef(fit))
  
  # save coefficients
  boot_coefs <- matrix(NA, nrow = B, ncol = length(coef(fit)))
  colnames(boot_coefs) <- coef_names
  
  for (b in 1:B) {
    # resample observation index
    idx <- sample(seq_len(n), size = n, replace = TRUE)
    data_b <- data[idx, , drop = FALSE]
    
    # refit
    fit_b <- lm(formula(fit), data = data_b)
    boot_coefs[b, ] <- coef(fit_b)
  }
  
  # Hitung bootstrap standard error
  # boot_se <- apply(boot_coefs, 2, sd, na.rm = TRUE)
  boot_cov <- cov(boot_coefs)
  
  # list(
  #   se = boot_se,
  #   boot_coefs = boot_coefs
  # )
  return(boot_cov)
}


# functions for residual bootstrap ----
res_boot <- function(model, B = 1000, seed = 123){
  set.seed(seed)

  # input
  X <- model.matrix(model)
  n <- nrow(X)
  coef0 <- coef(model)
  fit0 <- fitted(model)
  resid0 <- resid(model)

  # bootstrap
  boot_coefs <- matrix(NA, nrow=B, ncol=length(coef0))
  boot_t    <- matrix(NA, nrow=B, ncol=length(coef0))

  for(b in 1:B){
    # resample residuals with replacement
    res_star <- sample(resid0, size = n, replace = TRUE)
    # y* = y_hat + residual*
    y_star <- fit0 + res_star
    # refit model
    fit_star <- lm(y_star ~ X - 1) # model matrix (X) has column one as intercept
    bet <- coef(fit_star)
    boot_coefs[b,] <- bet
    # calculate t-stat
    se_star <- sqrt(diag(vcov(fit_star)))
    boot_t[b,] <- (bet - coef0) / se_star
  }
  colnames(boot_coefs) <- names(coef0)
  colnames(boot_t) <- names(coef0)

  return(
    list(
      boot_coef = as.data.frame(boot_coefs),
      boot_t = as.data.frame(boot_t)
    )
  )
}
