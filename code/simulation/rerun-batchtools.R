## rerun-batchtools

## switch it depending on where I run the script
batchh <- TRUE
if (batchh) { 
  setwd("~/simulations/")
  .libPaths("../Rlibs/")
}

# setwd("code/simulation/")
# To reduce run-time, set a lower number of replications (per scenario)
# n_simA <- 20
# set to value below for full replication
n_simA <- 500

true_sigmas      <- c(0.05, 0.5, 1)
name_true_sigmas <- c("verylow", "low", "high")
hshapes          <- paste0("hshape", 1:6)
ihshapes         <- 1:6

source("weight-functions-utils.R")

## run create-static-part.R
for (i in c(1:3)) {
  true_sigma      <- true_sigmas[[i]]
  name_true_sigma <- name_true_sigmas[[i]]
  source("create-static-part.R", echo=TRUE, local = TRUE)
}


purrr::map(c(1:6), 
           .f = function(i) purrr::map2(.x = true_sigmas, .y = name_true_sigmas,
                                        .f = function(true_sigma, name_true_sigma) {
                                          true_sigma <- true_sigma
                                          name_true_sigma <- name_true_sigma
                                          hshape  <- hshapes[[i]]
                                          ihshape <- ihshapes[[i]]
                                          source("setup-batch-wce-ranef-survival.R", echo=TRUE, local = TRUE)
                                          source("submit-jobs-wce-ranef-survival.R", echo=TRUE, local = TRUE)
                                        })
)

rm(list=ls())
gc()


