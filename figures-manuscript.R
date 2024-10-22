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
library(scales)
library(colorspace)
library(RSSthemes)

sim_path    <- "code/simulation/"
output_path <- "figures/"
dir.create(output_path, showWarnings = FALSE)


## load functions to present the results in graphical and tabular form
source(paste0(sim_path, "eval-utils.R"))
theme_set(theme_bw() +
            theme(
              axis.title   = element_text(size = rel(1.5)),
              axis.text    = element_text(size = rel(1.2)),
              legend.text  = element_text(size = rel(0.95)),
              legend.title = element_text(size = rel(1.9)),
              strip.text   = element_text(size = rel(1.4)),
              plot.title = element_text(hjust = .7, size = rel(1.5), face = "italic")))
true_sigmas      <- c(0.05, 0.5, 1)
name_true_sigmas <- c("verylow", "low", "high")
model_names      <- c("", "constrained_", "ridge_")
h_labels         <- c("(a)", "(b)", "(c)", "(d)")
l                <- c(20, 40, 100)

reg_path <- paste0(sim_path, "registry/wce-ranef-surv-registry_")



# Partial effects -------------------------------------------
model_labels     <- c("Uncons.", "Constr.", "Ridge")
for (l in l) {
  for (h in seq(h_labels)) { ## shapes: exponential decay, bi-linear, early peak and constant
    reg <- loadRegistry(paste0(reg_path, "hshape", h, "_l", l, "/"), work.dir = getwd())
    h_label <- h_labels[[h]]
    i <- 2 ## true_sigma == 2
    name_true_sigma <- name_true_sigmas[[i]]
    avg_simresults_df <- data.frame()
    for (j in seq(model_labels)) {
      model_name  <- model_names[[j]]
      model_label <- paste0(model_labels[[j]])
      id_pamm_wce_ped  <- findExperiments(prob.name = paste0("sim_wce_ranef_ped", h,"_", name_true_sigma, "_l", l), 
                                          algo.name = paste0("wce_ranef_", model_name, "ped"))
      res_pamm_wce_ped <- reduceResultsDataTable(ids=findDone(id_pamm_wce_ped[,1])) |>
        as_tibble() |>
        unnest("result")
      avg_simresults_df <- avg_simresults_df |> 
        bind_rows(avg_simresults(res_pamm_wce_ped, name = model_label))
      
      pxnsim_name <- paste0("pxnsim", h, j, "_l", l)
      assign(pxnsim_name, gg_pamm_xslice_nsims(res_pamm_wce_ped, model_name = model_label, sampled = TRUE) + 
               ylim(-0.04, 0.125))  ## this should change..
    }
  }
}

p <- wrap_elements(pxnsim11_l20 + pxnsim12_l20 + pxnsim13_l20 + plot_layout(ncol = 1) + plot_annotation(title = "(a) exponential decay")) +
  wrap_elements(pxnsim21_l20 + pxnsim22_l20 + pxnsim23_l20 + plot_layout(ncol = 1) + plot_annotation(title = "(b) bi-linear")) + 
  wrap_elements(pxnsim31_l20 + pxnsim32_l20 + pxnsim33_l20 + plot_layout(ncol = 1) + plot_annotation(title = "(c) early peak")) +
  wrap_elements(pxnsim41_l20 + pxnsim42_l20 + pxnsim43_l20 + plot_layout(ncol = 1) + plot_annotation(title = "(d) inverted U")) + 
  # pxnsim51 + pxnsim52 + pxnsim53 + 
  plot_layout(ncol = 4, byrow = FALSE)
p
ggsave(paste0(output_path, "sim_hshapes_all_l20.pdf"), p, width = 16, height = 9.2)
ggsave(paste0(output_path, "sim_hshapes_all_l20.eps"), p, device = cairo_ps, width = 16, height = 9.2)

p <- wrap_elements(pxnsim11_l40 + pxnsim12_l40 + pxnsim13_l40 + plot_layout(ncol = 1) + plot_annotation(title = "(a) exponential decay")) +
  wrap_elements(pxnsim21_l40 + pxnsim22_l40 + pxnsim23_l40 + plot_layout(ncol = 1) + plot_annotation(title = "(b) bi-linear")) + 
  wrap_elements(pxnsim31_l40 + pxnsim32_l40 + pxnsim33_l40 + plot_layout(ncol = 1) + plot_annotation(title = "(c) early peak")) +
  wrap_elements(pxnsim41_l40 + pxnsim42_l40 + pxnsim43_l40 + plot_layout(ncol = 1) + plot_annotation(title = "(d) inverted U")) + 
  # pxnsim51 + pxnsim52 + pxnsim53 + 
  plot_layout(ncol = 4, byrow = FALSE)
p
ggsave(paste0(output_path, "sim_hshapes_all_l40.pdf"), p, width = 16, height = 9.2)
ggsave(paste0(output_path, "sim_hshapes_all_l40.eps"), p, device = cairo_ps, width = 16, height = 9.2)


p <- wrap_elements(pxnsim11_l100 + pxnsim12_l100 + pxnsim13_l100 + plot_layout(ncol = 1) + plot_annotation(title = "(a) exponential decay")) +
  wrap_elements(pxnsim21_l100 + pxnsim22_l100 + pxnsim23_l100 + plot_layout(ncol = 1) + plot_annotation(title = "(b) bi-linear")) + 
  wrap_elements(pxnsim31_l100 + pxnsim32_l100 + pxnsim33_l100 + plot_layout(ncol = 1) + plot_annotation(title = "(c) early peak")) +
  wrap_elements(pxnsim41_l100 + pxnsim42_l100 + pxnsim43_l100 + plot_layout(ncol = 1) + plot_annotation(title = "(d) inverted U")) + 
  # pxnsim51 + pxnsim52 + pxnsim53 + 
  plot_layout(ncol = 4, byrow = FALSE)
p
p + labs(caption = expression("L = 100 players, "*sigma[b]*" = 0.5")) +
  theme(plot.caption = element_text(size = rel(1.2), colour = "gray25"))
ggsave(paste0(output_path, "sim_hshapes_all_l100.pdf"), last_plot(), width = 16, height = 9.2)
ggsave(paste0(output_path, "sim_hshapes_all_l100.eps"), p, device = cairo_ps, width = 16, height = 9.2)


rm(list = ls()[str_detect(ls(), "pxnsim")])
rm(list = c("p", "avg_simresults_df", "id_pamm_wce_ped", "res_pamm_wce_ped", "reg"))

# Boxplots of RMSE --------------------------------------------------------

model_labels   <- c("Uncons.", "Constr.", "Ridge") 
summaries_list <- vector("list", 3L)
for (ll in 1:3) {
  li <- l[[ll]]
  simsummary_df <- data.frame(data_generation1 = vector("character", 0L),
                              data_generation2 = vector("character", 0L),
                              data_generation3 = vector("character", 0L),
                              RMSE_h           = vector("double", 0L),
                              mse_sigma        = vector("double", 0L),
                              coverage_h       = vector("double", 0L),
                              job.id           = vector("double", 0L))
  ## initialize an empty list
  for (h in 1:4) {
    reg <- loadRegistry(paste0(reg_path, "hshape", h, "_l", li, "/"), work.dir = getwd())
    h_label <- h_labels[[h]]
    for (i in 1:3) {   ## sigmas
      name_true_sigma <- name_true_sigmas[[i]]
      true_sigma      <- true_sigmas[[i]]
      for (j in 1:3) { ## models
        model_name       <- model_names[[j]]
        model_label      <- model_labels[[j]]
        id_pamm_wce_ped  <- findExperiments(prob.name = paste0("sim_wce_ranef_ped", h, "_", name_true_sigma, "_l", li), 
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
  summaries_list[[ll]] <- simsummary_df
}

summaries_list <- map(summaries_list, function(simsummary_df) {
  simsummary_df <- simsummary_df |> 
    mutate(data_generation1_new = recode(data_generation1,
                                         "hshape*ztz 1" = "(a) exponential decay",
                                         "hshape*ztz 2" = "(b) bi-linear",
                                         "hshape*ztz 3" = "(c) early peak",
                                         "hshape*ztz 4" = "(d) inverted U"),
           data_generation3 = factor(data_generation3, 
                                     levels = c("Uncons.", "Constr.", "Ridge"), 
                                     labels = c("Uncons.", "Constr.", "Ridge")))
  return(simsummary_df)
})

ymax_vec <- map(summaries_list, function(simsummary_df) {
  ymax_vec <- summarize(simsummary_df,
                        max_rmse  = max(RMSE_h),
                        max_sigma = max(mse_sigma),
                        max_cov   = max(coverage_h))
}
) |> 
  reduce(rbind) |> 
  summarize(max_rmse  = max(max_rmse),
            max_sigma = max(max_sigma),
            max_cov   = max(max_cov))
ymax_vec[[1]] <- ymax_vec[[1]] + ymax_vec[[1]]*0.1 
ymax_vec[[2]] <- 4 
ymax_vec[[3]] <- 1

yvar      <- c("RMSE_h", "mse_sigma", "coverage_h")
yvarlabel <- c("RMSE (h)", expression("Squared Error ("*sigma[b]*")"), "Coverage (h)")


#Our transformation function
scaleFUN <- function(x) sprintf("%.2f", x)
theme_boxplots <- theme(legend.text  = element_text(size = rel(2), face = "italic"),
                        legend.title = element_text(size = rel(2)),
                        legend.key.size = unit(3,"line"),
                        axis.text.x = element_text(size = rel(0.9)),
                        plot.title  = element_text(size = rel(3), hjust = 0.5,
                                                   margin = margin(0, 0, 2, 0, unit = "lines")))
for (i in seq(length(summaries_list))) {
  yyvar <- yvar[[i]]
  yyvarlabel <- yvarlabel[[i]]
  
  p1 <- summaries_list[[1]] |> 
    ggplot(aes(x = factor(data_generation2), y = !!sym(yyvar), fill = data_generation3)) +
    geom_boxplot() + 
    {if (i == 3) geom_hline(yintercept = 0.95)} +
    facet_wrap(~data_generation1_new) +
    scale_fill_manual(values = models_palette, name = "Model:") +
    scale_x_discrete(labels = TeX(unique(simsummary_df$data_generation2))) +
    scale_y_continuous(labels = scaleFUN, limits = c(0, ymax_vec[[i]])) +
    labs(x = "Heterogeneity level", y = yyvarlabel) +
    theme_boxplots +
    {if(i == 1) ggtitle(paste0("L = ", l[[1]]))} +
    theme(legend.position = "bottom")
  
  p2 <- summaries_list[[2]] |> 
    ggplot(aes(x = factor(data_generation2), y = !!sym(yyvar), fill = data_generation3)) +
    geom_boxplot() + 
    {if (i == 3) geom_hline(yintercept = 0.95)} +
    facet_wrap(~data_generation1_new) +
    scale_fill_manual(values = models_palette, name = "Model:") +
    scale_x_discrete(labels = TeX(unique(simsummary_df$data_generation2))) +
    scale_y_continuous(labels = scaleFUN, limits = c(0, ymax_vec[[i]])) +
    labs(x = "Heterogeneity level", y = yyvarlabel) +
    {if (i == 1) ggtitle(paste0("L = ", l[[2]]))} +
    theme_boxplots
  
  p3 <- summaries_list[[3]] |> 
    ggplot(aes(x = factor(data_generation2), y = !!sym(yyvar), fill = data_generation3)) +
    geom_boxplot() + 
    {if (i == 3) geom_hline(yintercept = 0.95)} +
    facet_wrap(~data_generation1_new) +
    scale_fill_manual(values = models_palette, name = "Model:") +
    scale_x_discrete(labels = TeX(unique(simsummary_df$data_generation2))) +
    scale_y_continuous(labels = scaleFUN, limits = c(0, ymax_vec[[i]])) +
    labs(x = "Heterogeneity level", y = yyvarlabel) +
    {if (i == 1) ggtitle(paste0("L = ", l[[3]]))} +
    theme_boxplots
  
  p1_aux <- p1 + theme(axis.title.y = element_text(size = rel(1.5), vjust = 0.5)) ## rel(1.8)
  y_axis <- cowplot::get_plot_component(p1_aux, "ylab-l")
  
  design = "
  EABC
  "
  p_name <- paste0("p_", yyvar)
  assign(p_name, list(
    p1 + labs(x = NULL, y = NULL) + 
      theme(legend.position = "none"), # A
    p2 + labs(x = NULL, y = NULL) + 
      theme(legend.position = "none"), # B
    p3 + labs(x = NULL, y = NULL) + 
      theme(legend.position = "none"), # C
    #x_axis,# D
    y_axis # E
  ) |> 
    wrap_plots() + 
    plot_layout(heights = c(40), widths = c(8, 50, 50, 50), design = design)) |> 
    print()
}

p <- ggplot() + labs(x = "Heterogeneity level", y = yyvarlabel) + 
  theme(axis.title.y = element_text(size = rel(1.5), vjust = 0.5), ## rel(1.8)
        axis.title.x = element_text(size = rel(1.5), vjust = 1, margin = margin(1.5, 0, 2.5, 0, "lines")))
x_axis <- cowplot::get_plot_component(p, "xlab-b")
legend <- cowplot::get_plot_component(p1, "guide-box-bottom")

design = "
A
B
C
D
E
F
G
"
list(p_RMSE_h, # A
     ggplot() + geom_blank() + theme_void(), # B
     p_mse_sigma, # C
     ggplot() + geom_blank() + theme_void(), # D
     p_coverage_h, # E
     x_axis, # F
     legend) |>  # G
  wrap_plots() +
  plot_layout(widths = c(40), heights = c(30, 4, 30, 4, 30, 7, 5), design = design)
ggsave(paste0(output_path, "sim_boxplots_all.pdf"), plot = last_plot(), device = "pdf", width = 15.7, height = 15.7*1.35)
ggsave(paste0(output_path, "sim_boxplots_all.eps"), plot = last_plot(), device = "pdf", width = 15.7, height = 15.7*1.35)

graphics.off()



# SIMILAR Traspose --------------------------------------------------------
#Our transformation function
scaleFUN <- function(x) sprintf("%.2f", x)
theme_boxplots <- theme(legend.text = element_text(size = rel(2), face = "italic"),
                        legend.title = element_text(size = rel(2)),
                        legend.key.size = unit(3,"line"),
                        axis.text.x = element_text(size = rel(0.9)),
                        plot.title  = element_text(size = rel(2.2), hjust = 0.5,
                                                   margin = margin(0, 0, 2, 0, unit = "lines")))
for (i in seq(length(summaries_list))) {
  simsummary_df <- summaries_list[[i]]
  ll <- l[[i]]
  
  p1 <- simsummary_df |> 
    ggplot(aes(x = factor(data_generation2), y = RMSE_h, fill = data_generation3)) +
    geom_boxplot() +
    facet_wrap(~data_generation1_new) +
    scale_fill_manual(values = models_palette, name = "Model:") +
    scale_x_discrete(labels = TeX(unique(simsummary_df$data_generation2))) +
    scale_y_continuous(labels = scaleFUN) +
    labs(x = "Heterogeneity level", y = paste0("L = ", ll)) +
    theme_boxplots +
    {if(i == 1) ggtitle("RMSE (h)")} +
    theme(legend.position = "bottom")
  
  p2 <- simsummary_df |> 
    ggplot(aes(x = factor(data_generation2), y = mse_sigma, fill = data_generation3)) +
    geom_boxplot() +
    facet_wrap(~data_generation1_new) +
    scale_fill_manual(values = models_palette, name = "Model:") +
    scale_x_discrete(labels = TeX(unique(simsummary_df$data_generation2))) +
    {if (i == 1) scale_y_continuous(labels = scaleFUN, limits = c(0, ymax_vec[[2]]))} +
    scale_y_continuous(labels = scaleFUN) +
    labs(x = "Heterogeneity level", y = paste0("L = ", ll)) +
    {if (i == 1) ggtitle(expression("Squared Error ("*sigma[b]*")"))} +
    theme_boxplots
  
  p3 <- simsummary_df |> 
    ggplot(aes(x = factor(data_generation2), y = coverage_h, fill = data_generation3)) +
    geom_boxplot() + 
    geom_hline(yintercept = 0.95) +
    facet_wrap(~data_generation1_new) +
    scale_fill_manual(values = models_palette, name = "Model:") +
    scale_x_discrete(labels = TeX(unique(simsummary_df$data_generation2))) +
    scale_y_continuous(labels = scaleFUN) +
    labs(x = "Heterogeneity level", y = paste0("L = ", ll)) +
    {if (i == 1) ggtitle("Coverage (h)")} +
    theme_boxplots
  
  p1_aux <- p1 + theme(axis.title.y = element_text(size = rel(1.5), vjust = 0.5)) ## rel(1.8)
  y_axis <- cowplot::get_plot_component(p1_aux, "ylab-l")
  
  design = "
  EABC
  "
  p_name <- paste0("p_", ll)
  assign(p_name, list(
    p1 + labs(x = NULL, y = NULL) + 
      theme(legend.position = "none"), # A
    p2 + labs(x = NULL, y = NULL) + 
      theme(legend.position = "none"), # B
    p3 + labs(x = NULL, y = NULL) + 
      theme(legend.position = "none"), # C
    #x_axis,# D
    y_axis # E
  ) |> 
    wrap_plots() + 
    plot_layout(heights = c(40), widths = c(8, 50, 50, 50), design = design)) |> 
    print()
}

p <- ggplot() + labs(x = "Heterogeneity level") + 
  theme(axis.title.y = element_text(size = rel(1.8), vjust = 0.5), ## rel(1.8)
        axis.title.x = element_text(size = rel(1.5), vjust = 1, margin = margin(1.5, 0, 2.5, 0, "lines")))
x_axis <- cowplot::get_plot_component(p, "xlab-b")
legend <- cowplot::get_plot_component(p1, "guide-box-bottom")

design = "
A
B
C
D
E
F
G
"
list(p_20, # A
     ggplot() + geom_blank() + theme_void(), # B
     p_40, # C
     ggplot() + geom_blank() + theme_void(), # D
     p_100, # E
     x_axis, # F
     legend) |>  # G
  wrap_plots() +
  plot_layout(widths = c(40), heights = c(30, 4, 30, 4, 30, 7, 5), design = design)
ggsave(paste0(output_path, "sim_boxplots_all_trasposed.pdf"), plot = last_plot(), device = "pdf", width = 16.2, height = 17.1)

graphics.off()


