## rerun-batchtools

## switch it depending on where I run the script
batchh <- TRUE
if (batchh) { 
  setwd("/scratch/mperez/lore/simulations_royalC_round2/")
  .libPaths("/scratch/mperez/Rlibs/")
}

# To reduce run-time, set a lower number of replications (per scenario)
# n_simA <- 20
# set to value below for full replication
n_simA <- 500

true_sigmas        <- c(0.05, 0.5, 1)
names(true_sigmas) <- c("verylow", "low", "high")
hshapes            <- paste0("hshape", 1:4)
ihshapes           <- 1:4
l                  <- c(20, 40, 100)
# params_sub <- expand.grid(data.frame(true_sigmas = true_sigmas,
#                                      l = l)) |>
#   dplyr::mutate(names_true_sigmas = dplyr::case_when(true_sigmas == 0.05 ~ "verylow",
#                                                      true_sigmas == 0.5 ~ "low",
#                                                      true_sigmas == 1 ~ "high"))
params_sub <- data.frame(true_sigmas = rep(true_sigmas, each = 3),
                                     l = rep(l, 3)) |>
  dplyr::mutate(names_true_sigmas = dplyr::case_when(true_sigmas == 0.05 ~ "verylow",
                                                     true_sigmas == 0.5 ~ "low",
                                                     true_sigmas == 1 ~ "high"))

source("weight-functions-utils.R")

## run create-static-part.R
for (l in l) {
  for (i in seq(true_sigmas)) {
    true_sigma      <- true_sigmas[[i]]
    name_true_sigma <- names(true_sigmas)[[i]]
    source("create-static-part.R", echo=TRUE, local = TRUE)
  }
}


purrr::map(seq(hshapes), 
           .f = function(i) purrr::pmap(params_sub,
                                        .f = function(true_sigmas, l, names_true_sigmas) {
                                          true_sigma <- true_sigmas
                                          name_true_sigma <- names_true_sigmas
                                          hshape  <- hshapes[[i]]
                                          ihshape <- ihshapes[[i]]
                                          source("setup-batch-wce-ranef-survival.R", echo=TRUE, local = TRUE)
                                          source("submit-jobs-wce-ranef-survival.R", echo=TRUE, local = TRUE)
                                        })
)

rm(list=ls())
gc()


