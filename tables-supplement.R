## Tables of the supplementary material

library(batchtools)
library(ggplot2)
library(tidyr) ## unnest, tidyr
library(gridExtra)
library(knitr)
library(kableExtra)
library(latex2exp)
library(ggpubr)
library(stringr)
library(patchwork)

sim_path    <- "code/simulation/"
output_path <- "figures/supplement/"
dir.create(output_path, showWarnings = FALSE)

n_simA <- 500

## load functions to present the results in graphical and tabular form
source(paste0(sim_path, "eval-utils.R"))
theme_set(theme_bw() +
            theme(
              axis.title   = element_text(size = rel(1.3)),
              axis.text    = element_text(size = rel(1.2)),
              legend.text  = element_text(size = rel(0.8)),
              legend.title = element_text(size = rel(1.1)),
              strip.text   = element_text(size = rel(1.2)),
              plot.title = element_text(hjust = .6, size = rel(1.4), face = "italic")))
true_sigmas      <- c(0.05, 0.5, 1)
name_true_sigmas <- c("verylow", "low", "high")
model_names      <- c("", "constrained_", "ridge_")
model_labels     <- c("PAMM WCE\n", "PAMM WCE\nCONSTR.", "PAMM WCE\nRIDGE")
h_labels         <- c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)")

reg_path <- paste0(sim_path, "/registry/wce-ranef-surv-registry_")



# Table (accuracy and coverage measures) ----------------------------------
## initialize an empty list
simsummary_avg_df <- data.frame(data_generation1 = vector("character", 0L),
                                data_generation2 = vector("character", 0L),
                                data_generation3 = vector("character", 0L),
                                mRMSE_h          = vector("double", 0L),
                                RMSE_sigma      = vector("double", 0L),
                                mcoverage_h      = vector("double", 0L))
model_labels     <- c("PAMM WCE", "PAMM WCE CONSTR.", "PAMM WCE RIDGE")
for (h in 1:6) {
  reg <- loadRegistry(paste0(reg_path, "hshape", h, "/"), work.dir = getwd())
  h_label <- h_labels[[h]]
  for (i in 1:3) {   ## sigmas
    name_true_sigma <- name_true_sigmas[[i]]
    true_sigma      <- true_sigmas[[i]]
    for (j in 1:3) { ## models
      model_name       <- model_names[[j]]
      model_label      <- model_labels[[j]]
      id_pamm_wce_ped  <- findExperiments(prob.name = paste0("sim_wce_ranef_ped", h, "_", name_true_sigma), 
                                          algo.name = paste0("wce_ranef_", model_name, "ped"))
      res_pamm_wce_ped <- reduceResultsDataTable(ids=findDone(id_pamm_wce_ped[,1])) %>%
        as_tibble() %>%
        unnest("result")  
      
      sim_res <- simsummary_avg_pamm_wce(res_pamm_wce_ped, true_sigma)
      sim_res_aux <- data.frame(data_generation1 = paste0("hshape*ztz ", h),
                                data_generation2 = paste0("$\\sigma$ = ", true_sigma),
                                data_generation3 = paste0("model: ", model_label))
      simsummary_avg_df <- bind_rows(simsummary_avg_df,
                                     bind_cols(sim_res_aux, sim_res))
    }
  }
}



## LaTeX table with simulation results 
tab <- simsummary_avg_df %>% 
  mutate(data_generation3 = case_when(data_generation3 == "model: PAMM WCE" ~ "PAMM WCE",
                                      data_generation3 == "model: PAMM WCE CONSTR." ~ "PAMM WCE Constr.",
                                      data_generation3 == "model: PAMM WCE RIDGE" ~ "PAMM WCE Ridge"
  )) %>% 
  kbl(., booktabs = T, align = c("l", "l", "l", "c", "c", "c", "c"),
      caption = paste0("Simulation results for $N_{\\text{sim}} = ", 
                       n_simA, "$ in each scenario setting in terms of mean RMSE and mean coverage of $h_{t, t_z, z(t_z)}$ and RMSE of $\\sigma_b$\\label{tab:simres}"),
      col.names = c("WCE shape", "Heterogeneity", " ", "$h_{t, t_z, z(t_z)}$", "$\\sigma_b$", "$h_{t, t_z, z(t_z)}$"),
      format = "latex", digits = 3, escape = FALSE) %>% 
  kable_styling() %>% 
  add_header_above(c("Data generation mechanism" = 2, "Model" = 1, "Mean RMSE" = 2, "Mean Coverage" = 1)) %>% 
  # row_spec(0, bold = TRUE) %>% 
  collapse_rows(columns = 1, latex_hline = "none", valign = "middle") 
writeLines(tab, con = paste0(output_path, "simsummary_tab.tex"))


# Table (BIC, Dev. Explained) ---------------------------------------------
## initialize an empty list
aic_df <- data.frame()
model_labels     <- c("PAMM_WCE", "PAMM_WCE_CONSTR.", "PAMM_WCE_RIDGE")
for (h in 1:6) {
  reg <- loadRegistry(paste0(reg_path, "hshape", h, "/"), work.dir = getwd())
  h_label <- h_labels[[h]]
  for (i in 1:3) {   ## sigmas
    name_true_sigma <- name_true_sigmas[[i]]
    true_sigma      <- true_sigmas[[i]]
    models_aic_df   <- data.frame()
    for (j in 1:3) { ## models
      model_name       <- model_names[[j]]
      model_label      <- model_labels[[j]]
      id_pamm_wce_ped  <- findExperiments(prob.name = paste0("sim_wce_ranef_ped", h, "_", name_true_sigma), 
                                          algo.name = paste0("wce_ranef_", model_name, "ped"))
      res_pamm_wce_ped <- reduceResultsDataTable(ids=findDone(id_pamm_wce_ped[,1])) %>%
        as_tibble() %>%
        unnest("result")  
      
      reduced_res     <- rereduce(res_pamm_wce_ped, "aic", model_label)
      reduced_res_aux <- data.frame(data_generation1 = paste0("shape  ", h),
                                    data_generation2 = paste0("$\\sigma$ = ", true_sigma))
      reduced_res <- bind_cols(reduced_res_aux, reduced_res)
      if (j != 1) reduced_res$job.id <- reduced_res$job.id - 500*(3-j+1) ## cause i changed ridge and constrained order. Otherwise 500*(j-1)
      if (length(models_aic_df)) {
        models_aic_df <- left_join(models_aic_df, reduced_res, by = c("job.id", "data_generation1", "data_generation2"))
      } else {
        models_aic_df <- reduced_res 
      }
    }
    aic_df_aux <- best_aic_models(models_aic_df)
    aic_df <- bind_rows(aic_df, aic_df_aux)
  }
}

aic_df <- aic_df %>% 
  mutate(across(.cols = contains("PAMM"), .fn = ~paste0(., " (", round(./500*100), "\\%)")))

## LaTeX table with simulation results 
tab <- aic_df %>% 
  select(1,2,3,5, 4,6,8,7, 9,11,10) %>%
  select(contains("data"), contains("bic"), contains("dev")) %>% 
  mutate(across(.cols = contains("PAMM"), .fn =  ~str_match_all(., "(?<=\\().+?(?=\\))")),
         across(.cols = contains("PAMM"), .fn =  ~str_extract_all(., "\\d+"))) %>% 
  kbl(., booktabs = T, align = c("l", "l", "c", "c", "c", "c", "c", "c", "c", "c", "c"),
      caption = paste0("Frequency of the models that yield best BIC and Deviance explained in each replication of the simulation, $N_{\\text{sim}}$ = ",
                       n_simA, ", across all scenarios\\label{tab:bestmodels_small}."),
      col.names = c("WCE shape", "Heterogeneity",
                    "Lowest BIC PAMM (\\%)", "Lowest BIC PAMM Constr. (\\%)", "Lowest BIC PAMM Ridge (\\%)", 
                    "Largest Dev. PAMM (\\%)", "Largest Dev. PAMM Constr. (\\%)", "Largest Dev. PAMM Ridge (\\%)"),
      format = "latex", digits = 3, escape = FALSE) %>% 
  kable_styling() %>% 
  add_header_above(c("Data generation mechanism" = 2, " " = 6))
writeLines(tab, con = paste0(output_path, "bestmodels_tab.tex"))



