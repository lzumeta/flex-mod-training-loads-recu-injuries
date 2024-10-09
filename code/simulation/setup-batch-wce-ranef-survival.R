library(batchtools)
dir.create("registry/", showWarnings = F)


if (!dir.exists(paste0("registry/wce-ranef-surv-registry_", hshape, "_l", l))) {
  if (batchh) {
    reg <- makeExperimentRegistry(
      paste0("registry/wce-ranef-surv-registry_", hshape, "_l", l),
      conf.file = ".batchtools.conf.R",
      packages  = c("mgcv", "magrittr", "dplyr", "purrr", "pammtools", "ggplot2", "extraDistr"),
      source    = c("problems-wce-ranef-survival.R", "algorithms-wce-ranef-survival.R"),
      seed      = 19190124)
  } else {
    reg <- makeExperimentRegistry(
      paste0("registry/wce-ranef-surv-registry_", hshape, "_l", l),
      packages = c("mgcv", "magrittr", "dplyr", "purrr", "pammtools", "ggplot2", "extraDistr"),
      source   = c("problems-wce-ranef-survival.R", "algorithms-wce-ranef-survival.R"),
      seed     = 19190124) 
  }
} else {
  reg <- loadRegistry(file.dir = paste0("registry/wce-ranef-surv-registry_", hshape, "_l", l, "/"), writeable=TRUE,
                      work.dir = getwd(), conf.file = ".batchtools.conf.R")
}


# reg$cluster.functions = makeClusterFunctionsMulticore(ncpus=4)
saveRegistry()


#### wce ranef for survival data
addProblem(
  name = paste0("sim_wce_ranef_ped", ihshape, "_", name_true_sigma, "_l", l),
  data = readRDS(paste0("input/static_part_ped", ihshape, "_", name_true_sigma, "_l", l, ".Rds")),
  fun  = sim_wce_ranef_ped,
  seed = 20160618)
## setting a problem seed: the problem seed is incremented only depending on the
## experiment replication, so that different algorithms are evaluated on the
## same stochastic instance.

addAlgorithm(
  name = "wce_ranef_ped",
  fun  = wce_ranef_ped)
addAlgorithm(
  name = "wce_ranef_ridge_ped",
  fun  = wce_ranef_ridge_ped)
addAlgorithm(
  name = "wce_ranef_constrained_ped",
  fun  = wce_ranef_constrained_ped)
# addAlgorithm(
#   name = "wce_ranef_ad_ped",
#   fun  = wce_ranef_ad_ped)

# prob_list <- replicate(6, data.frame())
# prob_list <- setNames(prob_list,
#                       paste0("sim_wce_ranef_ped", 1:6, "_", name_true_sigma))
prob_list <- list(data.frame())
prob_list <- setNames(prob_list,
                      paste0("sim_wce_ranef_ped", ihshape, "_", name_true_sigma, "_l", l))
addExperiments(
  prob.designs = prob_list,
  algo.designs = list(wce_ranef_ped = data.frame(debug = FALSE),
                      wce_ranef_ridge_ped = data.frame(debug = FALSE),
                      wce_ranef_constrained_ped = data.frame(debug = FALSE)),
                      # wce_ranef_ad_ped = data.frame(debug = FALSE)),
  repls = n_simA)
