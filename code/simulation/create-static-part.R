## create-static-part

## Simulation scenario settings (WCE shapes) mostly from:
## Sylvestre et al. 2009
## https://onlinelibrary.wiley.com/doi/10.1002/sim.3701

## Reuse and adapt the simulation code structure from:
## Bender et al. 2018
## https://academic.oup.com/biostatistics/article/20/2/315/4852816?login=false
## https://github.com/adibender/elra-biostats

## packages
library(dplyr)
library(ggplot2)
library(dlnm)       ## data set: chicagoNMMAPS
library(tsModel)    ## tsModel::Lag()
library(extraDistr) ## extraDistr::rtpois()

input_path <- "./input/"
dir.create(input_path, showWarnings = FALSE)

# Common static part ------------------------------------------------------
## additional additive, non-linear component of the log-baseline hazard
f0 <- function(t) {
  t <- (t) / max(t) * 3/2*pi
  (-1*sin(t)) - 0.5
}
# FUNCTION TO COMPUTE THE CUMULATIVE EFFECT GIVEN AN EXPOSURE HISTORY
fcumeff <- function(hist, lag, fun) sum(do.call(fun, list(hist, lag)))

# REAL TEMPERATURE SERIES, STANDARDIZED IN 0-10
# chicagoNMMAPS data from dlnm package
xz <- chicagoNMMAPS$temp
xz <- (xz - min(xz))/diff(range(xz))*10

## set parameters
# number of exposures per subject
nz <- 80 # t_z = (1:80 - 41)

# number of subjects
l <- 500

# sample exposures from the empirical distribution of x
set.seed(16)
x <- sample(xz, nz*l, replace = T)
##

## id and time variables
time <- rep(1:nz, l)
id   <- rep(1:l, each = nz)
## calculate cumulative effect for all time-points and all subjects (= eta)
# each element of x_list contains exposure history with nz elements
x_list   <- split(x, id)
# each element of lag_list contains a Lag matrix, with lags 0 to 40
# lag = 1 means exposure was recorded one day before time at which we want to
# model the hazard, i.e. t-tz = 1
# lag = 40 means exposure was recorded 40 days before the time at which we want
# to model the hazard, i.e. t-tz = 40
lag_list <- lapply(x_list, Lag, 0:40)

## nl variable
## number of events per subject (fixed)
## drawn from rtpois with lower a = 0 truncation point, so nl is a realization
## of a poisson (fixed in simulations) random variable, Y, conditional on the
## event Y > a = 0
set.seed(16)
nl <- rtpois(l, lambda = 1.5, a = 0, b = Inf)
# summary(nl)
## N, total number of observations
## where observations consist of subjects under risk of an event i
N  <- sum(nl)  ## N = 961
nl_rep <- rep(nl, each = 40) ## duplicate nl nz=80 times (for Xdf)



# a) h(t-tz): exponential decay -------------------------------------------
# plot(1:40, hshape1(1:40), type = "l")
## the partial effect function, h(t, t_z, z(t_z)) = h(t-tz)*z(tz)
hshape1_ztz <- function(x, lag) x*hshape1(lag)

# For each subject, go through each row of the Lag matrix and calculate
# the cumulative effect for each lag.
# Note: this will be NA for the fist 39 rows, as for the WCE we need
# max(lag) observations at each time-point at which we want to model the hazard.
# The cumulative effect is the sum over all 40 partial effects, i.e.
# h(t-tz=1, z[tz=t-1]) + h(t-tz=2, z[tz=t-2]) + ... + h(t-tz=40, z[tz=t-40])
eff_vec  <- sapply(lag_list, apply, 1, fcumeff, 0:40, "hshape1_ztz")

## create full data set (observations for each subject and time-point)
Xdf <- data.frame(id = id, time = time, eta_wce = as.vector(eff_vec))
## create covariates for linear functionals
# these need to be matrices with repeated entries, for mgcv to recognize the
# specified terms as cumulative effects
# - Z = exposure history matrix
# - tz_df = matrix of exposure times t_z
# - time_df = matrix of time-points of the follow-up
# - LL = lag-lead matrix
Xdf$Z       <- do.call(rbind, lapply(x_list, matrix, nrow = nz, ncol = nz, byrow = TRUE))
Xdf$tz_df   <- matrix(1:nz, nrow = nrow(Xdf), ncol = nz, byrow = TRUE)
Xdf$time_df <- matrix(Xdf$time, nrow = nrow(Xdf), ncol = nz)
diff_df     <- (Xdf$time_df - Xdf$tz_df)
Xdf$LL      <- ((diff_df >= 0) & (diff_df <= 40))*1
set.seed(16)
Xdf$ranef   <- rep(rnorm(l, mean = 0, sd = true_sigma), each = 80)
## follow up starts after 40 days of exposure, such that every subject has
## complete exposure history of 40 exposures at the beginning of the follow-up
Xdf$time <- Xdf$time - 41
## remove all obs before follow-up starts
Xdf        <- Xdf[Xdf$time >= 0, ]
Xdf$tstart <- Xdf$time
Xdf$tend   <- Xdf$tstart + 1
## calculate linear predictor using:
## cumulative effect + intercept + smooth baseline + random effect
Xdf$eta   <- -7.7 + f0(Xdf$time) + Xdf$eta_wce + Xdf$ranef

## important: the above should be, or it is convenient to be, between the ranges
## since (we want the survival times to be approx in the range [0, 40]).
## Upon this will depend the censorship
# rexp(1, rate=exp(-4.2))
# rexp(1, rate=exp(-0.6))
# summary(Xdf$eta)

#### create new data for prediction + truth column
ndf <- expand.grid(Z = seq(0, 10, by = 0.25), tz_df = 0:40)
# any time will do (later we only use coefficients for hshape1_ztz estimation)
ndf$tend  <- 20
ndf$LL    <- 1
ndf$id    <- 1
ndf$truth <- apply(ndf, 1, function(x) {hshape1_ztz(x[1], x[2])})


## save static part of the simulation (wce and random effect, ped)
wce1_ranef_ped_static <- list(
  X        = Xdf,
  ndf      = ndf,
  nl       = nl,
  nl_rep   = nl_rep)
saveRDS(wce1_ranef_ped_static, paste0(input_path, "static_part_ped1_", name_true_sigma, ".Rds"))




# b) h(t-tz): bi-linear ---------------------------------------------------
# plot(1:40, hshape2(1:40), type = "l")
## the partial effect function, h(t, t_z, z(t_z)) = h(t-tz)*z(tz)
hshape2_ztz <- function(x, lag) x*hshape2(lag)


# For each subject, go through each row of the Lag matrix and calculate
# the cumulative effect for each lag.
# Note: this will be NA for the fist 39 rows, as for the WCE we need
# max(lag) observations at each time-point at which we want to model the hazard.
# The cumulative effect is the sum over all 40 partial effects, i.e.
# h(t-tz=1, z[tz=t-1]) + h(t-tz=2, z[tz=t-2]) + ... + h(t-tz=40, z[tz=t-40])
eff_vec  <- sapply(lag_list, apply, 1, fcumeff, 0:40, "hshape2_ztz")

## create full data set (observations for each subject and time-point)
Xdf <- data.frame(id = id, time = time, eta_wce = as.vector(eff_vec))
## create covariates for linear functionals
# these need to be matrices with repeated entries, for mgcv to recognize the
# specified terms as cumulative effects
# - Z = exposure history matrix
# - tz_df = matrix of exposure times t_z
# - time_df = matrix of time-points of the follow-up
# - LL = lag-lead matrix
Xdf$Z       <- do.call(rbind, lapply(x_list, matrix, nrow = nz, ncol = nz, byrow = TRUE))
Xdf$tz_df   <- matrix(1:nz, nrow = nrow(Xdf), ncol = nz, byrow = TRUE)
Xdf$time_df <- matrix(Xdf$time, nrow = nrow(Xdf), ncol = nz)
diff_df     <- (Xdf$time_df - Xdf$tz_df)
Xdf$LL      <- ((diff_df >= 0) & (diff_df <= 40))*1
set.seed(16)
Xdf$ranef   <- rep(rnorm(l, mean = 0, sd = true_sigma), each = 80)

## follow up starts after 40 days of exposure, such that every subject has
## complete exposure history of 40 exposures at the beginning of the follow-up
Xdf$time <- Xdf$time - 41
## remove all obs before follow-up starts
Xdf        <- Xdf[Xdf$time >= 0, ]
Xdf$tstart <- Xdf$time
Xdf$tend   <- Xdf$tstart + 1
## calculate linear predictor using:
## cumulative effect + intercept + smooth baseline + random effect
Xdf$eta   <- -5 + f0(Xdf$time) + Xdf$eta_wce + Xdf$ranef

## important: the above should be, or it is convenient to be, between the ranges
## since (we want the survival times to be approx in the range [0, 40]).
## Upon this will depend the censorship
# rexp(1, rate=exp(-4.2))
# rexp(1, rate=exp(-0.6))
# summary(Xdf$eta)

#### create new data for prediction + truth column
ndf <- expand.grid(Z = seq(0, 10, by = 0.25), tz_df = 0:40)
# any time will do (later we only use coefficients for hshape1_ztz estimation)
ndf$tend  <- 20
ndf$LL    <- 1
ndf$id    <- 1
ndf$truth <- apply(ndf, 1, function(x) {hshape2_ztz(x[1], x[2])})


## save static part of the simulation (wce and random effect, ped)
wce2_ranef_ped_static <- list(
  X        = Xdf,
  ndf      = ndf,
  nl       = nl,
  nl_rep   = nl_rep)
saveRDS(wce2_ranef_ped_static, paste0(input_path, "static_part_ped2_", name_true_sigma, ".Rds"))




# c) h(t-tz): early peak --------------------------------------------------
# plot(1:40, hshape3(1:40), type = "l")
## the partial effect function, h(t, t_z, z(t_z)) = h(t-tz)*z(tz)
hshape3_ztz <- function(x, lag) x*hshape3(lag)


# For each subject, go through each row of the Lag matrix and calculate
# the cumulative effect for each lag.
# Note: this will be NA for the fist 39 rows, as for the WCE we need
# max(lag) observations at each time-point at which we want to model the hazard.
# The cumulative effect is the sum over all 40 partial effects, i.e.
# h(t-tz=1, z[tz=t-1]) + h(t-tz=2, z[tz=t-2]) + ... + h(t-tz=40, z[tz=t-40])
eff_vec  <- sapply(lag_list, apply, 1, fcumeff, 0:40, "hshape3_ztz")

## create full data set (observations for each subject and time-point)
Xdf <- data.frame(id = id, time = time, eta_wce = as.vector(eff_vec))
## create covariates for linear functionals
# these need to be matrices with repeated entries, for mgcv to recognize the
# specified terms as cumulative effects
# - Z = exposure history matrix
# - tz_df = matrix of exposure times t_z
# - time_df = matrix of time-points of the follow-up
# - LL = lag-lead matrix
Xdf$Z       <- do.call(rbind, lapply(x_list, matrix, nrow = nz, ncol = nz, byrow = TRUE))
Xdf$tz_df   <- matrix(1:nz, nrow = nrow(Xdf), ncol = nz, byrow = TRUE)
Xdf$time_df <- matrix(Xdf$time, nrow = nrow(Xdf), ncol = nz)
diff_df     <- (Xdf$time_df - Xdf$tz_df)
Xdf$LL      <- ((diff_df >= 0) & (diff_df <= 40))*1
set.seed(16)
Xdf$ranef   <- rep(rnorm(l, mean = 0, sd = true_sigma), each = 80)

## follow up starts after 40 days of exposure, such that every subject has
## complete exposure history of 40 exposures at the beginning of the follow-up
Xdf$time <- Xdf$time - 41
## remove all obs before follow-up starts
Xdf        <- Xdf[Xdf$time >= 0, ]
Xdf$tstart <- Xdf$time
Xdf$tend   <- Xdf$tstart + 1
## calculate linear predictor using:
## cumulative effect + intercept + smooth baseline + random effect
Xdf$eta   <- -4 + f0(Xdf$time) + Xdf$eta_wce + Xdf$ranef

## important: the above should be, or it is convenient to be, between the ranges
## since (we want the survival times to be approx in the range [0, 40]).
## Upon this will depend the censorship
# rexp(1, rate=exp(-4.2))
# rexp(1, rate=exp(-0.6))
# summary(Xdf$eta)

#### create new data for prediction + truth column
ndf <- expand.grid(Z = seq(0, 10, by = 0.25), tz_df = 0:40)
# any time will do (later we only use coefficients for hshape1_ztz estimation)
ndf$tend  <- 20
ndf$LL    <- 1
ndf$id    <- 1
ndf$truth <- apply(ndf, 1, function(x) {hshape3_ztz(x[1], x[2])})


## save static part of the simulation (wce and random effect, ped)
wce3_ranef_ped_static <- list(
  X        = Xdf,
  ndf      = ndf,
  nl       = nl,
  nl_rep   = nl_rep)
saveRDS(wce3_ranef_ped_static, paste0(input_path, "static_part_ped3_", name_true_sigma, ".Rds"))




# d) h(t-tz): inverted U --------------------------------------------------
# plot(1:40, hshape4(1:40), type = "l")
## the partial effect function, h(t, t_z, z(t_z)) = h(t-tz)*z(tz)
hshape4_ztz <- function(x, lag) x*hshape4(lag)


# For each subject, go through each row of the Lag matrix and calculate
# the cumulative effect for each lag.
# Note: this will be NA for the fist 39 rows, as for the WCE we need
# max(lag) observations at each time-point at which we want to model the hazard.
# The cumulative effect is the sum over all 40 partial effects, i.e.
# h(t-tz=1, z[tz=t-1]) + h(t-tz=2, z[tz=t-2]) + ... + h(t-tz=40, z[tz=t-40])
eff_vec  <- sapply(lag_list, apply, 1, fcumeff, 0:40, "hshape4_ztz")

## create full data set (observations for each subject and time-point)
Xdf <- data.frame(id = id, time = time, eta_wce = as.vector(eff_vec))
## create covariates for linear functionals
# these need to be matrices with repeated entries, for mgcv to recognize the
# specified terms as cumulative effects
# - Z = exposure history matrix
# - tz_df = matrix of exposure times t_z
# - time_df = matrix of time-points of the follow-up
# - LL = lag-lead matrix
Xdf$Z       <- do.call(rbind, lapply(x_list, matrix, nrow = nz, ncol = nz, byrow = TRUE))
Xdf$tz_df   <- matrix(1:nz, nrow = nrow(Xdf), ncol = nz, byrow = TRUE)
Xdf$time_df <- matrix(Xdf$time, nrow = nrow(Xdf), ncol = nz)
diff_df     <- (Xdf$time_df - Xdf$tz_df)
Xdf$LL      <- ((diff_df >= 0) & (diff_df <= 40))*1
set.seed(16)
Xdf$ranef   <- rep(rnorm(l, mean = 0, sd = true_sigma), each = 80)

## follow up starts after 40 days of exposure, such that every subject has
## complete exposure history of 40 exposures at the beginning of the follow-up
Xdf$time <- Xdf$time - 41
## remove all obs before follow-up starts
Xdf        <- Xdf[Xdf$time >= 0, ]
Xdf$tstart <- Xdf$time
Xdf$tend   <- Xdf$tstart + 1
## calculate linear predictor using:
## cumulative effect + intercept + smooth baseline + random effect
Xdf$eta   <- -5.7 + f0(Xdf$time) + Xdf$eta_wce + Xdf$ranef

## important: the above should be, or it is convenient to be, between the ranges
## since (we want the survival times to be approx in the range [0, 40]).
## Upon this will depend the censorship
# rexp(1, rate=exp(-4.2))
# rexp(1, rate=exp(-0.6))
# summary(Xdf$eta)

#### create new data for prediction + truth column
ndf <- expand.grid(Z = seq(0, 10, by = 0.25), tz_df = 0:40)
# any time will do (later we only use coefficients for hshape1_ztz estimation)
ndf$tend  <- 20
ndf$LL    <- 1
ndf$id    <- 1
ndf$truth <- apply(ndf, 1, function(x) {hshape4_ztz(x[1], x[2])})


## save static part of the simulation (wce and random effect, ped)
wce4_ranef_ped_static <- list(
  X        = Xdf,
  ndf      = ndf,
  nl       = nl,
  nl_rep   = nl_rep)
saveRDS(wce4_ranef_ped_static, paste0(input_path, "static_part_ped4_", name_true_sigma, ".Rds"))




# e) h(t-tz): constant ----------------------------------------------------
# plot(1:40, hshape5(1:40), type = "l")
## the partial effect function, h(t, t_z, z(t_z)) = h(t-tz)*z(tz)
hshape5_ztz <- function(x, lag) x*hshape5(lag)


# For each subject, go through each row of the Lag matrix and calculate
# the cumulative effect for each lag.
# Note: this will be NA for the fist 39 rows, as for the WCE we need
# max(lag) observations at each time-point at which we want to model the hazard.
# The cumulative effect is the sum over all 40 partial effects, i.e.
# h(t-tz=1, z[tz=t-1]) + h(t-tz=2, z[tz=t-2]) + ... + h(t-tz=40, z[tz=t-40])
eff_vec  <- sapply(lag_list, apply, 1, fcumeff, 0:40, "hshape5_ztz")

## create full data set (observations for each subject and time-point)
Xdf <- data.frame(id = id, time = time, eta_wce = as.vector(eff_vec))
## create covariates for linear functionals
# these need to be matrices with repeated entries, for mgcv to recognize the
# specified terms as cumulative effects
# - Z = exposure history matrix
# - tz_df = matrix of exposure times t_z
# - time_df = matrix of time-points of the follow-up
# - LL = lag-lead matrix
Xdf$Z       <- do.call(rbind, lapply(x_list, matrix, nrow = nz, ncol = nz, byrow = TRUE))
Xdf$tz_df   <- matrix(1:nz, nrow = nrow(Xdf), ncol = nz, byrow = TRUE)
Xdf$time_df <- matrix(Xdf$time, nrow = nrow(Xdf), ncol = nz)
diff_df     <- (Xdf$time_df - Xdf$tz_df)
Xdf$LL      <- ((diff_df >= 0) & (diff_df <= 40))*1
set.seed(16)
Xdf$ranef   <- rep(rnorm(l, mean = 0, sd = true_sigma), each = 80)

## follow up starts after 40 days of exposure, such that every subject has
## complete exposure history of 40 exposures at the beginning of the follow-up
Xdf$time <- Xdf$time - 41
## remove all obs before follow-up starts
Xdf        <- Xdf[Xdf$time >= 0, ]
Xdf$tstart <- Xdf$time
Xdf$tend   <- Xdf$tstart + 1
## calculate linear predictor using:
## cumulative effect + intercept + smooth baseline + random effect
Xdf$eta   <- -5 + f0(Xdf$time) + Xdf$eta_wce + Xdf$ranef

## important: the above should be, or it is convenient to be, between the ranges
## since (we want the survival times to be approx in the range [0, 40]).
## Upon this will depend the censorship
# rexp(1, rate=exp(-4.2))
# rexp(1, rate=exp(-0.6))
# summary(Xdf$eta)

#### create new data for prediction + truth column
ndf <- expand.grid(Z = seq(0, 10, by = 0.25), tz_df = 0:40)
# any time will do (later we only use coefficients for hshape1_ztz estimation)
ndf$tend  <- 20
ndf$LL    <- 1
ndf$id    <- 1
ndf$truth <- apply(ndf, 1, function(x) {hshape5_ztz(x[1], x[2])})


## save static part of the simulation (wce and random effect, ped)
wce5_ranef_ped_static <- list(
  X        = Xdf,
  ndf      = ndf,
  nl       = nl,
  nl_rep   = nl_rep)
saveRDS(wce5_ranef_ped_static, paste0(input_path, "static_part_ped5_", name_true_sigma, ".Rds"))




# f) h(t-tz): hat ---------------------------------------------------------
# plot(1:40, hshape6(1:40), type = "l")
## the partial effect function, h(t, t_z, z(t_z)) = h(t-tz)*z(tz)
hshape6_ztz <- function(x, lag) x*hshape6(lag)


# For each subject, go through each row of the Lag matrix and calculate
# the cumulative effect for each lag.
# Note: this will be NA for the fist 39 rows, as for the WCE we need
# max(lag) observations at each time-point at which we want to model the hazard.
# The cumulative effect is the sum over all 40 partial effects, i.e.
# h(t-tz=1, z[tz=t-1]) + h(t-tz=2, z[tz=t-2]) + ... + h(t-tz=40, z[tz=t-40])
eff_vec  <- sapply(lag_list, apply, 1, fcumeff, 0:40, "hshape6_ztz")

## create full data set (observations for each subject and time-point)
Xdf <- data.frame(id = id, time = time, eta_wce = as.vector(eff_vec))
## create covariates for linear functionals
# these need to be matrices with repeated entries, for mgcv to recognize the
# specified terms as cumulative effects
# - Z = exposure history matrix
# - tz_df = matrix of exposure times t_z
# - time_df = matrix of time-points of the follow-up
# - LL = lag-lead matrix
Xdf$Z       <- do.call(rbind, lapply(x_list, matrix, nrow = nz, ncol = nz, byrow = TRUE))
Xdf$tz_df   <- matrix(1:nz, nrow = nrow(Xdf), ncol = nz, byrow = TRUE)
Xdf$time_df <- matrix(Xdf$time, nrow = nrow(Xdf), ncol = nz)
diff_df     <- (Xdf$time_df - Xdf$tz_df)
Xdf$LL      <- ((diff_df >= 0) & (diff_df <= 40))*1
set.seed(16)
Xdf$ranef   <- rep(rnorm(l, mean = 0, sd = true_sigma), each = 80)

## follow up starts after 40 days of exposure, such that every subject has
## complete exposure history of 40 exposures at the beginning of the follow-up
Xdf$time <- Xdf$time - 41
## remove all obs before follow-up starts
Xdf        <- Xdf[Xdf$time >= 0, ]
Xdf$tstart <- Xdf$time
Xdf$tend   <- Xdf$tstart + 1
## calculate linear predictor using:
## cumulative effect + intercept + smooth baseline + random effect
Xdf$eta   <- -6 + f0(Xdf$time) + Xdf$eta_wce + Xdf$ranef

## important: the above should be, or it is convenient to be, between the ranges
## since (we want the survival times to be approx in the range [0, 40]).
## Upon this will depend the censorship
# rexp(1, rate=exp(-4.2))
# rexp(1, rate=exp(-0.6))
# summary(Xdf$eta)

#### create new data for prediction + truth column
ndf <- expand.grid(Z = seq(0, 10, by = 0.25), tz_df = 0:40)
# any time will do (later we only use coefficients for hshape1_ztz estimation)
ndf$tend  <- 20
ndf$LL    <- 1
ndf$id    <- 1
ndf$truth <- apply(ndf, 1, function(x) {hshape6_ztz(x[1], x[2])})


## save static part of the simulation (wce and random effect, ped)
wce6_ranef_ped_static <- list(
  X        = Xdf,
  ndf      = ndf,
  nl       = nl,
  nl_rep   = nl_rep)
saveRDS(wce6_ranef_ped_static, paste0(input_path, "static_part_ped6_", name_true_sigma, ".Rds"))


# # Plot true WCE-cumulative effects ----------------------------------------
# plot_wce_cumueff <- function(ndf) {
#   ggplot(ndf, aes(x = .data$Z, y = .data$tz_df)) +
#     geom_tile(aes(fill = .data$truth)) +
#     geom_contour(aes(z = .data$truth)) +
#     scale_fill_gradient2(low = "firebrick", high = "steelblue")
# }
#
# for (i in 1:6) {
#   data <- readRDS(paste0("input/static_part_ped", i,"_high.Rds"))
#   ndf <- data$ndf
#   name_p <- paste0("p", i)
#   assign(name_p, plot_wce_cumueff(ndf))
# }
#
# theme_set(theme_bw())
# p_all <- gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3)
# ggsave("./true_WCE_cumu_effects.pdf", plot = p_all, width = 12, height = 7.6)
