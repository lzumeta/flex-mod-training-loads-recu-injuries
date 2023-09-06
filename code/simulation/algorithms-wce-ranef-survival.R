#### PAM WCE RANEF algorithm
wce_ranef_ped <- function(
    job,
    data,
    instance,
    cen   = 0,
    debug = FALSE) {
  
  require(mgcv)
  require(dplyr)
  
  mod <- gam(
    ped_status ~ s(tend, k  = 10, bs = "ps") + 
      s(tz_df, by = Z*LL, k = 15, bs = "ps") + s(id, bs = "re"),
    data = instance, family = poisson(), offset = offset, control = list(trace = TRUE), method = "REML")
  
  ndf    <- ndf2 <- data$ndf
  ndf2$Z <- cen
  X1     <- predict(mod, newdata = ndf, type = "lpmatrix")[, -1]  ## Z ranges from 0 to 10 
  X      <- X1
  X2     <- predict(mod, newdata = ndf2, type = "lpmatrix")[, -1] ## Z fixed to 0
  X      <- X1 - X2
  ndf    <- ndf %>% select(Z, tz_df, LL, everything())
  colnames(ndf)[1:3] <- c("x", "lag", "LL")
  
  vcomps <- gam.vcomp(mod)
  scale  <- mod$scale       ## equivalent of mod$sig2 (estimated or supplied variance/scale parameter)
  lambda_id <- mod$sp[[3]]
  
  ## store the results
  ndf$fit <- as.numeric(X %*% coef(mod)[-1])
  ndf$se  <- sqrt(rowSums((X %*% mod$Vp[-1,-1]) * X))
  ndf     <- ndf %>%  mutate(
    lag2       = lag,
    lag        = paste0("lag", lag),        
    sigma_est  = sqrt(scale/lambda_id),
    aic        = mod$aic,
    dev_expl   = (mod$null.deviance - mod$deviance)/mod$null.deviance * 100,
    bic        = BIC(mod))
  # sigma_est  =  vcomps[nrow(vcomps), 1],  
  # sigma_cilo =  vcomps[nrow(vcomps), 2],  
  # sigma_ciup =  vcomps[nrow(vcomps), 3])  
  
  gc()
  
  if(debug) {
    return(list(mod = mod, ndf = ndf))
  } else {
    return(ndf)
  }
  
}

## To add an extra penalty, create a 'fdl', constructor as in:
## Obermeier et al. 2015 (adapted code)
## https://www.jstor.org/stable/24771900
# *) Prepare a constructor for flexible distributed lags within the framework 
# of mgcv, only used internally by mgcv

smooth.construct.fdl.smooth.spec <- function(object, data, knots){
  
  #modify object so that it's fitted as a p-spline signal regression term:
  object$bs <- "ps"
  object <- smooth.construct.ps.smooth.spec(object, data, knots)
  
  if(!is.null(object$xt$ridge) && object$xt$ridge) {
    # add ridge penalty to last <degree of B-spline>+1 (=m+2) basis functions 
    # penalty = coef' (lambda_1*DiffPen + lambda_2*RidgePen) coef
    object$S[[2]] <- matrix(0, object$bs.dim, object$bs.dim)
    indx <- rev(seq.int(from = object$bs.dim, by = -1, length.out = object$m[1] + 2))
    object$S[[2]][cbind(indx, indx)] <- 1
    # object$S[[2]][cbind(1:(object$m[1]+2), 1:(object$m[1]+2))] <- 1
    
    object$rank <- c(object$rank, object$m[1] + 2)
  }
  
  if(!is.null(object$xt$constrain) && object$xt$constrain) {
    ## optionally one can constrain the last lag coefficient to be zero, 
    ## not recommended as we favor a soft, data-driven shrinkage to a hard constraint!
    #constrain to end in zero (i.e (X%*%coefficients)[1] == 0)
    # --> Constraint matrix C = X[1,]
    
    # object$C <- matrix(object$X[nrow(object$X), ], nrow = 1)
    # object$C <- structure(object$C, always.apply = TRUE)
    
    object$C <- matrix(object$X[1, ], nrow = 1)
    object$C <- structure(object$C, always.apply = TRUE)
  }
  
  return(object)
}

#### PAM WCE RANEF RIDGE algorithm
wce_ranef_ridge_ped <- function(
    job,
    data,
    instance,
    cen   = 0,
    debug = FALSE) {
  
  require(mgcv)
  require(dplyr)
  
  mod <- gam(
    ped_status ~ s(tend, k = 10, bs = "ps") + 
      s(tz_df, by = Z*LL, k = 15, bs = "fdl", xt = list(constrain = FALSE, ridge = TRUE)) +
      s(id, bs = "re"),
    data = instance, family = poisson(), offset = offset, control = list(trace=TRUE), method = "REML")
  
  ndf    <- ndf2 <- data$ndf
  ndf2$Z <- cen
  X1     <- predict(mod, newdata = ndf, type = "lpmatrix")[, -1]  ## Z ranges from 0 to 10
  X      <- X1
  X2     <- predict(mod, newdata = ndf2, type = "lpmatrix")[, -1] ## Z fixed to 0
  X      <- X1 - X2
  ndf    <- ndf %>% select(Z, tz_df, LL, everything())
  colnames(ndf)[1:3] <- c("x", "lag", "LL")
  
  vcomps <- gam.vcomp(mod)
  scale  <- mod$scale       ## equivalent of mod$sig2 (estimated or supplied variance/scale parameter)
  lambda_id <- mod$sp[[4]]
  
  ## store the results
  ndf$fit <- as.numeric(X %*% coef(mod)[-1])
  ndf$se  <- sqrt(rowSums((X %*% mod$Vp[-1,-1]) * X))
  ndf     <- ndf %>%  mutate(
    lag2       = lag,
    lag        = paste0("lag", lag),        
    sigma_est  = sqrt(scale/lambda_id),
    aic        = mod$aic,
    dev_expl   = (mod$null.deviance - mod$deviance)/mod$null.deviance * 100,
    bic        = BIC(mod))
  
  gc()
  
  if(debug) {
    return(list(mod = mod, ndf = ndf))
  } else {
    return(ndf)
  }
  
}

#### PAM WCE RANEF CONSTRAINED algorithm
wce_ranef_constrained_ped <- function(
    job,
    data,
    instance,
    cen   = 0,
    debug = FALSE) {
  
  require(mgcv)
  require(dplyr)
  
  mod <- gam(
    ped_status ~ s(tend, k = 10, bs = "ps") + 
      s(tz_df, by = Z*LL, k = 15, bs = "fdl", xt = list(constrain = TRUE, ridge = FALSE)) + 
      s(id, bs = "re"),
    data = instance, family = poisson(), offset = offset, control = list(trace = TRUE), method = "REML")
  
  ndf    <- ndf2 <- data$ndf
  ndf2$Z <- cen
  X1     <- predict(mod, newdata = ndf, type = "lpmatrix")[, -1]  ## Z ranges from 0 to 10
  X      <- X1
  X2     <- predict(mod, newdata = ndf2, type = "lpmatrix")[, -1] ## Z fixed to 0 
  X      <- X1 - X2
  ndf    <- ndf %>% select(Z, tz_df, LL, everything())
  colnames(ndf)[1:3] <- c("x", "lag", "LL")
  
  vcomps <- gam.vcomp(mod)
  scale  <- mod$scale       ## equivalent of mod$sig2 (estimated or supplied variance/scale parameter)
  lambda_id <- mod$sp[[3]]
  
  ## store the results
  ndf$fit <- as.numeric(X %*% coef(mod)[-1])
  ndf$se  <- sqrt(rowSums((X %*% mod$Vp[-1,-1]) * X))
  ndf     <- ndf %>%  mutate(
    lag2       = lag,
    lag        = paste0("lag", lag),        
    sigma_est  = sqrt(scale/lambda_id),
    aic        = mod$aic,
    dev_expl   = (mod$null.deviance - mod$deviance)/mod$null.deviance * 100,
    bic        = BIC(mod))
  
  gc()
  
  if(debug) {
    return(list(mod = mod, ndf = ndf))
  } else {
    return(ndf)
  }
  
}


#### PAM WCE RANEF Adaptive splines
wce_ranef_ad_ped <- function(
    job,
    data,
    instance,
    cen   = 0,
    debug = FALSE) {
  
  require(mgcv)
  require(dplyr)
  
  mod <- gam(
    ped_status ~ s(tend, k = 10, bs = "ps") + 
      s(tz_df, by = Z*LL, k = 15, m = 6, bs = "ad") + ## by default p-splines are used
      s(id, bs = "re"),
    data = instance, family = poisson(), offset = offset, control = list(trace = TRUE), method = "REML")
  
  ndf    <- ndf2 <- data$ndf
  ndf2$Z <- cen
  X1     <- predict(mod, newdata = ndf, type = "lpmatrix")[, -1]  ## Z ranges from 0 to 10
  X      <- X1
  X2     <- predict(mod, newdata = ndf2, type = "lpmatrix")[, -1] ## Z fixed to 0
  X      <- X1 - X2
  ndf    <- ndf %>% select(Z, tz_df, LL, everything())
  colnames(ndf)[1:3] <- c("x", "lag", "LL")
  
  vcomps <- gam.vcomp(mod)
  scale  <- mod$scale       ## equivalent of mod$sig2 (estimated or supplied variance/scale parameter)
  lambda_id <- mod$sp[[length(mod$sp)]]
  
  ## store the results
  ndf$fit <- as.numeric(X %*% coef(mod)[-1])
  ndf$se  <- sqrt(rowSums((X %*% mod$Vp[-1,-1]) * X))
  ndf     <- ndf %>%  mutate(
    lag2       = lag,
    lag        = paste0("lag", lag),        
    sigma_est  = sqrt(scale/lambda_id),
    aic        = mod$aic,
    dev_expl   = (mod$null.deviance - mod$deviance)/mod$null.deviance * 100,
    bic        = BIC(mod))
  
  gc()
  
  if(debug) {
    return(list(mod = mod, ndf = ndf))
  } else {
    return(ndf)
  }
  
}
