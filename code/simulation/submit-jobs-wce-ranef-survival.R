## load registry
library(batchtools)
options(batchtools.progress = FALSE)
reg <- loadRegistry(paste0("wce-ranef-surv-registry_", hshape, "/"), writeable = TRUE, 
                    work.dir = getwd(), conf.file = ".batchtools.conf.R")

## set multicore or other parallel options
# reg$cluster.functions  <- makeClusterFunctionsMulticore(ncpus = 4)


## PAM WCE RANEF for WCE RANEF survival data
exp_wce_ranef_ped <- findExperiments(
  prob.name = paste0("sim_wce_ranef_ped", ihshape, "_", name_true_sigma),
  algo.name = "wce_ranef_ped")

# t1_pam_wce_ranef1 <- testJob(id = exp_wce_ranef_ped1[1,1])
submitJobs(ids = findNotDone(exp_wce_ranef_ped[,1]),
           reg=reg, resources=list(partition="bcam-exclusive", walltime=60*8, ntasks=1, ncpus=4, memory=500)) ## memory = 1024*5
waitForJobs()

## PAM WCE RIDGE RANEF for WCE RANEF survival data
exp_wce_ridge_ranef_ped <- findExperiments(
  prob.name = paste0("sim_wce_ranef_ped", ihshape, "_", name_true_sigma),
  algo.name = "wce_ranef_ridge_ped")

# t1_pam_wce_ridge_ranef1 <- testJob(id = exp_wce_ridge_ranef_ped1[1,1])
submitJobs(ids = findNotDone(exp_wce_ridge_ranef_ped[,1]),
           reg=reg, resources=list(partition="bcam-exclusive", walltime=60*8, ntasks=1, ncpus=4, memory=500)) 
waitForJobs()

## PAM WCE CONSTRAINED RANEF for WCE RANEF survival data
exp_wce_constrained_ranef_ped <- findExperiments(
  prob.name = paste0("sim_wce_ranef_ped", ihshape, "_", name_true_sigma),
  algo.name = "wce_ranef_constrained_ped")

# t1_pam_wce_constrained_ranef1 <- testJob(id = exp_wce_constrained_ranef_ped1[1,1])
submitJobs(ids = findNotDone(exp_wce_constrained_ranef_ped[,1]),
           reg=reg, resources=list(partition="bcam-exclusive", walltime=60*8, ntasks=1, ncpus=4, memory=500)) 
waitForJobs()

## PAM WCE Adaptive splines RANEF for WCE RANEF survival data
exp_wce_ad_ranef_ped <- findExperiments(
  prob.name = paste0("sim_wce_ranef_ped", ihshape, "_", name_true_sigma),
  algo.name = "wce_ranef_ad_ped")

# t1_pam_wce_ad_ranef1 <- testJob(id = exp_wce_ad_ranef_ped1[1,1])
submitJobs(ids = findNotDone(exp_wce_ad_ranef_ped[,1]),
           reg=reg, resources=list(partition="bcam-exclusive", walltime=60*8, ntasks=1, ncpus=4, memory=500)) 
waitForJobs()

