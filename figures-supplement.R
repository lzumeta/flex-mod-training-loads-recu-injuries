## Figures of the supplementary material

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


## load functions to present the results in graphical and tabular form
source(paste0(sim_path, "weight-functions-utils.R"))
source(paste0(sim_path, "eval-utils.R"))
true_sigmas      <- c(0.05, 0.5, 1)
name_true_sigmas <- c("verylow", "low", "high")
model_names      <- c("", "constrained_", "ridge_")
h_labels         <- c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)")

reg_path <- paste0(sim_path, "/registry/wce-ranef-surv-registry_")


# Weight functions (ALL) --------------------------------------------------
setEPS()
postscript(file = paste0(output_path, "/weight_functions.eps"), width = 12, height = 7.6)
par(mfrow = c(2,3), cex = 1, cex.axis = 1.2, cex.main = 1.4, cex.lab = 1.5, mar = c(5, 5.5, 4, 2))
lags <- 1:40
plot(hshape1(lags), las = 1, type = "l", ylim = c(-0.0, 0.086),
     xlab = expression('lag: t - t'[z]),  ylab = expression('h(t - t'[z]*')'),
     main = "    Exponential decay")
mtext("(a)", 3, 2, adj = 0, cex = 1.5, font = 2)
plot(hshape2(lags), las = 1, type = "l", ylim = c(-0.0, 0.086),
     xlab = expression('lag: t - t'[z]), ylab = expression('h(t - t'[z]*')'),
     main = "Bi-linear")
mtext("(b)", 3, 2, adj = 0, cex = 1.5,  font = 2)
plot(hshape3(lags), las = 1, type = "l", ylim = c(-0.0, 0.086),
     xlab = expression('lag: t - t'[z]), ylab = expression('h(t - t'[z]*')'),
     main = "Early peak")
mtext("(c)", 3, 2, adj = 0, cex = 1.5, font = 2)
plot(hshape4(lags), las = 1, type = "l", ylim = c(-0.0, 0.086),
     xlab = expression('lag: t - t'[z]), ylab = expression('h(t - t'[z]*')'),
     main = "Inverted U")
mtext("(d)", 3, 2, adj = 0, cex = 1.5, font = 2)
plot(hshape5(lags), las = 1, type = "l", ylim = c(-0.0, 0.086),
     xlab = expression('lag: t - t'[z]), ylab = expression('h(t - t'[z]*')'),
     main = "Constant")
mtext("(e)", 3, 2, adj = 0, cex = 1.5, font = 2)
plot(hshape6(lags), las = 1, type = "l", ylim = c(-0.0, 0.086),
     xlab = expression('lag: t - t'[z]), ylab = expression('h(t - t'[z]*')'),
     main = "Hat")
mtext("(f)", 3, 2, adj = 0, cex = 1.5, font = 2)
dev.off()


# Weight functions (indv) -------------------------------------------------
lags <- 1:40
setEPS()
postscript(file = paste0(output_path, "weight_function_a.eps"), width = 6, height = 5)
par(cex.main = 3.28)
plot(hshape1(lags), las = 1, type = "l", ylim = c(-0.0, 0.086),
     bty = "n", yaxt = "n", xaxt = "n", xlab = "", ylab = "",
     main = "(a) Exponential decay")
dev.off()
setEPS()
postscript(file = paste0(output_path, "weight_function_b.eps"), width = 6, height = 5)
par(cex.main = 3.5)
plot(hshape2(lags), las = 1, type = "l", ylim = c(-0.0, 0.086),
     bty = "n", yaxt = "n", xaxt = "n", xlab = "", ylab = "",
     main = "(b) Bi-linear")
dev.off()
setEPS()
postscript(file = paste0(output_path, "weight_function_c.eps"), width = 6, height = 5)
par(cex.main = 3.5)
plot(hshape3(lags), las = 1, type = "l", ylim = c(-0.0, 0.086),
     bty = "n", yaxt = "n", xaxt = "n", xlab = "", ylab = "",
     main = "(c) Early peak")
dev.off()
setEPS()
postscript(file = paste0(output_path, "weight_function_d.eps"), width = 6, height = 5)
par(cex.main = 3.5)
plot(hshape4(lags), las = 1, type = "l", ylim = c(-0.0, 0.086),
     bty = "n", yaxt = "n", xaxt = "n", xlab = "", ylab = "",
     main = "(d) Inverted U")
dev.off()
setEPS()
postscript(file = paste0(output_path, "weight_function_e.eps"), width = 6, height = 5)
par(cex.main = 3.5)
plot(hshape5(lags), las = 1, type = "l", ylim = c(-0.0, 0.086),
     bty = "n", yaxt = "n", xaxt = "n", xlab = "", ylab = "",
     main = "(e) Constant")
dev.off()
setEPS()
postscript(file = paste0(output_path, "weight_function_f.eps"), width = 6, height = 5)
par(cex.main = 3.5)
plot(hshape6(lags), las = 1, type = "l", ylim = c(-0.0, 0.086),
     bty = "n", yaxt = "n", xaxt = "n", xlab = "", ylab = "",
     main = "(f) Hat")
dev.off()

# Partial effects (ALL) ---------------------------------------------------
model_labels     <- c("Uncons.", "Constr.", "Ridge")
for (h in 1:6) {
  reg <- loadRegistry(paste0(reg_path, "hshape", h, "/"), work.dir = getwd())
  h_label <- h_labels[[h]]
  for (i in 1:3) {
    name_true_sigma <- name_true_sigmas[[i]]
    avg_simresults_df <- data.frame()
    for (j in 1:3) {
      model_name  <- model_names[[j]]
      model_label <- model_labels[[j]]
      id_pamm_wce_ped  <- findExperiments(prob.name = paste0("sim_wce_ranef_ped", h,"_", name_true_sigma), 
                                          algo.name = paste0("wce_ranef_", model_name, "ped"))
      res_pamm_wce_ped <- reduceResultsDataTable(ids=findDone(id_pamm_wce_ped[,1])) |>
        as_tibble() |>
        unnest("result")
      avg_simresults_df <- avg_simresults_df |> 
        bind_rows(avg_simresults(res_pamm_wce_ped, name = model_label))
      
      pxnsim_name <- paste0("pxnsim", i, j)
      assign(pxnsim_name, gg_pamm_xslice_nsims(res_pamm_wce_ped, model_name = model_label, sampled = T) + 
               ylim(-0.03, 0.1) +   ## these limits could be others (i.e. needs to be generalized)
               theme(axis.text = element_text(size = rel(1.6)),
                     axis.title = element_text(size = rel(1.8)),
                     legend.text = element_text(size = rel(1.1))))
    }
    
    avg_simresults_df <- unique(avg_simresults_df)
    pxnsim_name <- paste0("pxnsim", i)
    assign(pxnsim_name, arrangeGrob(get(paste0("pxnsim", i, "1")), 
                                    get(paste0("pxnsim", i, "2")),
                                    get(paste0("pxnsim", i, "3")), ncol = 1))
  }
  
  pslicexnsim <- grid.arrange(arrangeGrob(pxnsim1, top = grid::textGrob(expression(sigma[b]*'= 0.05'), gp = grid::gpar(fontsize = rel(24)))),
                              arrangeGrob(pxnsim2, top = grid::textGrob(expression(sigma[b]*'= 0.5'), gp = grid::gpar(fontsize = rel(24)))),
                              arrangeGrob(pxnsim3, top = grid::textGrob(expression(sigma[b]*'= 1'), gp = grid::gpar(fontsize = rel(24)))), ncol = 3, 
                              top = grid::textGrob(bquote(.(h_label)*" h"[.(h)]*"(t-t"[z]*")*z(t"[z]*")"), 
                                                   gp=grid::gpar(fontsize=rel(28), fontface = "bold")),
                              padding = unit(4, "line"))
  
  
  ggsave(paste0(output_path, "slice_xnsim_", h,".pdf"), plot = pslicexnsim, device = "pdf", width = 15, height = 12.8)
  ggsave(paste0(output_path, "slice_xnsim_", h,".eps"), plot = pslicexnsim, device = cairo_ps, width = 15, height = 12.8)
}



# Boxplots (accuracy and coverage) ----------------------------------------
## initialize an empty list
simsummary_df <- data.frame(data_generation1 = vector("character", 0L),
                            data_generation2 = vector("character", 0L),
                            data_generation3 = vector("character", 0L),
                            RMSE_h           = vector("double", 0L),
                            mse_sigma        = vector("double", 0L),
                            coverage_h       = vector("double", 0L),
                            job.id           = vector("double", 0L))
model_labels     <- c("Uncons.", "Constr.", "Ridge") 
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
                                       "hshape*ztz 4" = "(d) inverted U",
                                       "hshape*ztz 5" = "(e) constant",
                                       "hshape*ztz 6" = "(f) hat"),
         data_generation3 = factor(data_generation3, 
                                   levels = c("Uncons.", "Constr.", "Ridge"), 
                                   labels = c("Uncons.", "Constr.", "Ridge")))

simsummary_df |> 
  ggplot(aes(x = factor(data_generation2), y = RMSE_h, fill = data_generation3)) +
  geom_boxplot() + 
  facet_wrap(~data_generation1_new) +
  scale_fill_manual(values = c("#009E73", "#F0E442", "#0072B2"), name = "Model:") +
  scale_x_discrete(labels = TeX(unique(simsummary_df$data_generation2))) +
  xlab("Heterogeneity level") + ylab("RMSE (h)") +
  theme(axis.text = element_text(size = rel(2)),
        axis.title = element_text(size = rel(2.4)),
        legend.title = element_text(size = rel(2.4)),
        legend.text = element_text(size = rel(2.4)),
        strip.text = element_text(size = rel(2)))
# ggtitle("Boxplots of RMSE (h) in each scenario setting")
ggsave(paste0(output_path, "p_res_boxplot_h.eps"), plot = last_plot(),   device = cairo_ps(), width = 18, height = 9.6)

simsummary_df |> 
  ggplot(aes(x = factor(data_generation2), y = mse_sigma, fill = data_generation3)) +
  geom_boxplot() + 
  facet_wrap(~data_generation1_new) +
  scale_fill_manual(values = c("#009E73", "#F0E442", "#0072B2"), name = "Model:") +
  scale_x_discrete(labels = TeX(unique(simsummary_df$data_generation2))) +
  xlab("Heterogeneity level") + ylab(bquote("Squared Error ("*sigma[b]*")")) +
  theme(axis.text = element_text(size = rel(2)),
        axis.title = element_text(size = rel(2.4)),
        legend.title = element_text(size = rel(2.4)),
        legend.text = element_text(size = rel(2.4)),
        strip.text = element_text(size = rel(2)))
# ggtitle("Boxplots of Squared Error (sigma) in each scenario setting")
ggsave(paste0(output_path, "p_res_boxplot_sigma.eps"), plot = last_plot(), device = cairo_ps(), width = 18, height = 9.6)

simsummary_df |> 
  ggplot(aes(x = factor(data_generation2), y = coverage_h, fill = data_generation3)) +
  geom_boxplot() + 
  geom_hline(yintercept = 0.95) +
  facet_wrap(~data_generation1_new) +
  scale_fill_manual(values = c("#009E73", "#F0E442", "#0072B2"), name = "Model:") +
  scale_x_discrete(labels = TeX(unique(simsummary_df$data_generation2))) +
  xlab("Heterogeneity level") + ylab("Coverage (h)") +
  theme(axis.text = element_text(size = rel(2)),
        axis.title = element_text(size = rel(2.4)),
        legend.title = element_text(size = rel(2.4)),
        legend.text = element_text(size = rel(2.4)),
        strip.text = element_text(size = rel(2)))
# ggtitle("Boxplots of Coverage (h) in each scenario setting")
ggsave(paste0(output_path, "p_res_boxplot_cov_h.eps"), plot = last_plot(), device = cairo_ps(), width = 18, height = 9.6)



# Coverage for each lag point ---------------------------------------------
## initialize an empty list
simsummary_avg_coverages_df <- data.frame(data_generation1 = vector("character", 0L),
                                          data_generation2 = vector("character", 0L),
                                          data_generation3 = vector("character", 0L),
                                          mRMSE_h          = vector("double", 0L),
                                          RMSE_sigma      = vector("double", 0L),
                                          mcoverage_h      = vector("double", 0L))
model_labels     <- c("PAMM WCE", "PAMM WCE CONSTR.", "PAMM WCE RIDGE")
h_labels2 <- c("(a) exponential decay", "(b) bi-linear", "(c) early peak",
               "(d) inverted U", "(e) constant", "(f) hat")
for (h in 1:6) {
  reg <- loadRegistry(paste0(reg_path, "hshape", h, "/"), work.dir = getwd())
  h_label <- h_labels2[[h]]
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
      
      sim_res <- simsummary_avg_coverages(res_pamm_wce_ped, true_sigma)
      sim_res_aux <- data.frame(data_generation1 = paste0("hshape*ztz ", h),
                                data_generation2 = paste0("$\\sigma_b$ = ", true_sigma),
                                data_generation3 = paste0("model: ", model_label))
      simsummary_avg_coverages_df <- bind_rows(simsummary_avg_coverages_df,
                                               bind_cols(sim_res_aux, sim_res))
    }
    p_pointcov_one_name <- paste0("p_pointcov_", i)
    assign(p_pointcov_one_name, gg_av_coverages(simsummary_avg_coverages_df, true_sigma))
  }
  p_pointcov_shape_name <- paste0("p_pointcov_hshape", h)
  assign(p_pointcov_shape_name,
         p_pointcov_1 + p_pointcov_2 + p_pointcov_3 +
           plot_layout(ncol = 3, guides = "collect") + 
           plot_annotation(title = h_label, 
                           theme = theme(plot.title = element_text(size = rel(1.6), hjust = 0.4))))
  ggsave(filename = paste0(output_path, p_pointcov_shape_name, ".eps"), 
         plot = get(p_pointcov_shape_name),
         device = cairo_ps(), width = 13, height = 4.8)
}

## Put together shapes 1-3
wrap_elements(p_pointcov_hshape1) + 
  wrap_elements(p_pointcov_hshape2) + wrap_elements(p_pointcov_hshape3) + plot_layout(ncol = 1)
ggsave(filename = paste0(output_path, "p_pointcov_hshapes1-3.eps"), 
       plot = last_plot(),
       device = cairo_ps(), width = 13, height = 14)

## Put together shape 4-6
wrap_elements(p_pointcov_hshape4) + 
  wrap_elements(p_pointcov_hshape5) + wrap_elements(p_pointcov_hshape6) + plot_layout(ncol = 1)
ggsave(filename = paste0(output_path, "p_pointcov_hshapes4-6.eps"), 
       plot = last_plot(),
       device = cairo_ps(), width = 13, height = 14)


graphics.off()
