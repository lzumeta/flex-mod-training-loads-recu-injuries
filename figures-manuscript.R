## Main manuscript figures

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
output_path <- "figures/"
dir.create(output_path, showWarnings = FALSE)


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
h_labels         <- c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)")

reg_path <- paste0(sim_path, "/registry/wce-ranef-surv-registry_")



# Partial effects -------------------------------------------
model_labels     <- c("Uncons.", "Constr.", "Ridge")
for (h in c(1:4)) { ## shapes: exponential decay, bi-linear, early peak and constant
  reg <- loadRegistry(paste0(reg_path, "hshape", h, "/"), work.dir = getwd())
  h_label <- h_labels[[h]]
  i <- 3 ## true_sigma == 3
  name_true_sigma <- name_true_sigmas[[i]]
  avg_simresults_df <- data.frame()
  for (j in 1:3) {
    model_name  <- model_names[[j]]
    model_label <- paste0(model_labels[[j]])
    id_pamm_wce_ped  <- findExperiments(prob.name = paste0("sim_wce_ranef_ped", h,"_", name_true_sigma), 
                                        algo.name = paste0("wce_ranef_", model_name, "ped"))
    res_pamm_wce_ped <- reduceResultsDataTable(ids=findDone(id_pamm_wce_ped[,1])) |>
      as_tibble() |>
      unnest("result")
    avg_simresults_df <- avg_simresults_df |> 
      bind_rows(avg_simresults(res_pamm_wce_ped, name = model_label))
    
    pxnsim_name <- paste0("pxnsim", h, j)
    assign(pxnsim_name, gg_pamm_xslice_nsims(res_pamm_wce_ped, model_name = model_label, sampled = TRUE) + 
             ylim(-0.02, 0.1))  ## this should change..
  }
}

p <- wrap_elements(pxnsim11 + pxnsim12 + pxnsim13 + plot_layout(ncol = 1) + plot_annotation(title = "(a) exponential decay")) +
  wrap_elements(pxnsim21 + pxnsim22 + pxnsim23 + plot_layout(ncol = 1) + plot_annotation(title = "(b) bi-linear")) + 
  wrap_elements(pxnsim31 + pxnsim32 + pxnsim33 + plot_layout(ncol = 1) + plot_annotation(title = "(c) early peak")) +
  wrap_elements(pxnsim41 + pxnsim42 + pxnsim43 + plot_layout(ncol = 1) + plot_annotation(title = "(d) inverted U")) + 
  # pxnsim51 + pxnsim52 + pxnsim53 + 
  plot_layout(ncol = 4, byrow = FALSE)
p

ggsave(paste0(output_path, "sim_hshapes_all.pdf"), p, width = 16, height = 9.2)
ggsave(paste0(output_path, "sim_hshapes_all.eps"), p, device = cairo_ps, width = 16, height = 9.2)



# Boxplots of RMSE --------------------------------------------------------
## initialize an empty list
simsummary_df <- data.frame(data_generation1 = vector("character", 0L),
                            data_generation2 = vector("character", 0L),
                            data_generation3 = vector("character", 0L),
                            RMSE_h           = vector("double", 0L),
                            mse_sigma        = vector("double", 0L),
                            coverage_h       = vector("double", 0L),
                            job.id           = vector("double", 0L))
model_labels     <- c("Uncons.", "Constr.", "Ridge") 
for (h in 1:4) {
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
      res_pamm_wce_ped <- reduceResultsDataTable(ids=findDone(id_pamm_wce_ped[,1])) |>
        as_tibble() |>
        unnest("result")  
      
      sim_res <- simsummary_pamm_wce(res_pamm_wce_ped, true_sigma)
      sim_res_aux <- data.frame(data_generation1 = paste0("hshape*ztz ", h),
                                data_generation2 = paste0("$\\sigma_b$ = ", true_sigma),
                                data_generation3 = model_label)
      simsummary_df <- bind_rows(simsummary_df,
                                 bind_cols(sim_res_aux, sim_res))
    }
  }
}

simsummary_df <- simsummary_df |> 
  mutate(data_generation1_new = recode(data_generation1,
                                       "hshape*ztz 1" = "(a) exponential decay",
                                       "hshape*ztz 2" = "(b) bi-linear",
                                       "hshape*ztz 3" = "(c) early peak",
                                       "hshape*ztz 4" = "(d) inverted U"),
         data_generation3 = factor(data_generation3, 
                                   levels = c("Uncons.", "Constr.", "Ridge"), 
                                   labels = c("Uncons.", "Constr.", "Ridge")))

p1 <- simsummary_df |> 
  ggplot(aes(x = factor(data_generation2), y = RMSE_h, fill = data_generation3)) +
  geom_boxplot() + 
  facet_wrap(~data_generation1_new) +
  scale_fill_manual(values = c("#009E73", "#F0E442", "#0072B2"), name = "Model:") +
  scale_x_discrete(labels = TeX(unique(simsummary_df$data_generation2))) +
  xlab("Heterogeneity level") + ylab("RMSE (h)") + 
  theme(legend.text = element_text(size = rel(1.3)),
        axis.text.x = element_text(size = rel(0.9)))

p2 <- simsummary_df |> 
  ggplot(aes(x = factor(data_generation2), y = mse_sigma, fill = data_generation3)) +
  geom_boxplot() + 
  facet_wrap(~data_generation1_new) +
  scale_fill_manual(values = c("#009E73", "#F0E442", "#0072B2"), name = "Model:") +
  scale_x_discrete(labels = TeX(unique(simsummary_df$data_generation2))) +
  xlab("Heterogeneity level") + ylab(expression("Squared Error ("*sigma[b]*")")) +
  theme(legend.text = element_text(size = rel(1.3)),
        axis.text.x = element_text(size = rel(0.9)))

p3 <- simsummary_df |> 
  ggplot(aes(x = factor(data_generation2), y = coverage_h, fill = data_generation3)) +
  geom_boxplot() + 
  geom_hline(yintercept = 0.95) +
  facet_wrap(~data_generation1_new) +
  scale_fill_manual(values = c("#009E73", "#F0E442", "#0072B2"), name = "Model:") +
  scale_x_discrete(labels = TeX(unique(simsummary_df$data_generation2))) +
  xlab("Heterogeneity level") + ylab("Coverage (h)") + 
  theme(legend.text = element_text(size = rel(1.3)),
        axis.text.x = element_text(size = rel(0.9)))

p1 + p2 + p3 + plot_layout(guides = "collect")
ggsave(paste0(output_path, "sim_boxplots.pdf"), plot = last_plot(), device = "pdf", width = 18, height = 5.7)
ggsave(paste0(output_path, "sim_boxplots.eps"), plot = last_plot(), device = cairo_ps, width = 18, height = 5.7)


graphics.off()
