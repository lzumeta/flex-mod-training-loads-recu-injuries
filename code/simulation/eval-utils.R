theme_set(theme_bw() +
            theme(
              axis.title   = element_text(size = rel(1.3)),
              axis.text    = element_text(size = rel(1.2)),
              legend.text  = element_text(size = rel(0.8)),
              legend.title = element_text(size = rel(1.1)),
              strip.text   = element_text(size = rel(1.2)),
              plot.title = element_text(hjust = .6, size = rel(1.4), face = "italic")))


avg_simresults <- function(res_sim_df, name = "PAMM (WCE)") {
  res_sim_df %>%
    group_by(.data$x, .data$lag2) %>%
    summarize_at(vars(fit, truth), mean) %>%
    arrange(x, lag2) %>%
    gather(type, value, fit:truth) %>% 
    mutate(type = case_when(
      type == "fit" ~ name,
      TRUE ~ "TRUTH")) %>% 
    ungroup() %>% 
    ## change levels for plots (preferred order for panels + dashed lines)
    mutate(type = factor(type, levels = c("TRUTH", name))) 
}

gg_av_pamm_wce <- function(res_avg_data) {
  res_avg_data %>%  
    ggplot(aes(x = .data$x, y = .data$lag2)) +
    geom_tile(aes(fill = .data$value)) +
    geom_contour(aes(z = .data$value), col = "grey70") +
    scale_fill_gradient2(
      name = expression(h(t - t[z]) * z(t[z])),
      low = "firebrick", high = "steelblue") +
    facet_wrap(~.data$type) + 
    ylab(expression(t - t[z])) + xlab(expression(z(t[z]))) +
    theme(legend.position = "bottom")
}


gg_pamm_xslice <- function(res_avg_data, xs = c(2.5, 5, 7.5)) { ## xs param is not generalized yet
  res_avg_data %>% 
    filter(.data$x %in% xs) %>%
    pivot_wider(., id_cols = c("type", "lag2"), names_from = c("x"), values_from = c("value")) %>% 
    ggplot(aes(x = .data$lag2, group = .data$type, linetype = .data$type)) +
    geom_line(aes(y = `2.5`, col = "red")) +
    geom_line(aes(y = `5`, col = "purple")) + 
    geom_line(aes(y = `7.5`, col = "orange")) +
    ylab(expression(hat(h)(t-t[z])*z(t[z]))) + xlab(expression(t-t[z])) +
    scale_color_manual(name = "Covariate", values = c("orange", "purple", "red"), ## watch out with the order of these two vectors (values and labels)
                       labels = c(expression('z(t'[z]*') = 7.5'), expression('z(t'[z]*') = 5'), expression('z(t'[z]*') = 2.5'))) +
    scale_linetype(name = "Model fit")
}


gg_pamm_lagslice <- function(res_avg_data, lags = c(5, 10, 30)) { ## lags param is not generalized yet
  res_avg_data %>% 
    filter(.data$lag2 %in% lags) %>%
    pivot_wider(., id_cols = c("type", "x"), names_from = c("lag2"), values_from = c("value")) %>% 
    ggplot(aes(x = .data$x, group = .data$type, linetype = .data$type)) +
    geom_line(aes(y = `5`, col = "red")) +
    geom_line(aes(y = `10`, col = "purple")) + 
    geom_line(aes(y = `30`, col = "orange")) +
    ylab(expression(hat(h)(t-t[z])*z(t[z]))) + xlab(expression(z(t[z]))) +
    scale_color_manual(name = "Lag", values = c("orange", "purple", "red"), ## watch out with the order of these two vectors (values and labels)
                       labels = c(expression('t-t'[z]*' = 30'), expression('t-t'[z]*' = 10'), expression('t-t'[z]*' = 5'))) + 
    scale_linetype(name = "Model fit")
}


gg_pamm_xslice_nsims <- function(res_sim_df, xs = c(1), model_name, sampled = FALSE) { 
  set.seed(16)
  ids <- sample(1:500, 50)
  job.ids <- unique(res_sim_df$job.id)[ids]
  
  data <- res_sim_df %>% 
    filter(.data$x == xs)
  data_avg <- data %>% 
    group_by(.data$lag2) %>% 
    summarize(avg_curve = mean(fit), .groups = "keep") %>% 
    ungroup()
  
  if (sampled) data <- data %>% filter(job.id %in% job.ids)
  data %>% 
    ggplot(aes(x = .data$lag2)) +
    geom_line(aes(y = fit, group = job.id, col = "grey"), alpha = 0.4) +
    geom_line(aes(y = truth, col = "black"), linewidth = 1) +
    geom_line(aes(y = avg_curve, col = "red"), linewidth = 1,
              data = data_avg) +
    geom_hline(yintercept = 0, linetype = 2) +
    ylab(expression(hat(h)(t-t[z])*z(t[z]))) + xlab(expression(t-t[z])) +
    scale_color_manual(name = "", values = c("black", "grey", "red"),
                       labels = c("Truth", model_name, "Mean")) +
    theme(legend.position = c(0.79, 0.88),#c(0.8, 0.9), 
          legend.background = element_blank())
}


simsummary_avg_pamm_wce  <- function(res_sim_df, true_sigma) {
  summary_res <- res_sim_df %>%
    mutate(mse       = (fit - truth)^2,
           coverage  = (truth <= fit + qnorm(0.975)*se) & (truth >= fit - qnorm(0.975)*se)) %>%
    group_by(job.id) %>%
    summarise(RMSE = sqrt(mean(mse)),
              coverage = sum(coverage)/n()) %>%
    ungroup() %>% 
    summarise(mRMSE_h = mean(RMSE),
              mcoverage_h = mean(coverage))
  summary_res_aux <- res_sim_df %>%
    select(job.id, sigma_est) %>% #, sigma_cilo, sigma_ciup) %>% 
    group_by(job.id) %>% 
    distinct() %>% 
    ungroup() %>% 
    mutate(mse_sigma = (sigma_est - true_sigma)^2) %>% 
    # cov_sigma = (true_sigma <= sigma_ciup) & (true_sigma >= sigma_cilo)) %>% 
    summarise(RMSE_sigma = sqrt(mean((mse_sigma))))
  # coverage_sigma  = sum(cov_sigma)/n())
  
  summary_res <- bind_cols(summary_res, summary_res_aux) %>% 
    select(starts_with("mRMSE"), starts_with("RMSE"), starts_with("mcov"))
  return(summary_res)
}


simsummary_pamm_wce  <- function(res_sim_df, true_sigma) {
  summary_res <- res_sim_df %>%
    mutate(mse      = (fit - truth)^2,
           coverage = (truth <= fit + qnorm(0.975)*se) & (truth >= fit - qnorm(0.975)*se)) %>%
    group_by(job.id) %>%
    summarise(RMSE_h    = sqrt(mean(mse)),
              coverage_h = sum(coverage)/n()) %>%
    ungroup()
  summary_res_aux <- res_sim_df %>%
    select(job.id, sigma_est) %>% #, sigma_cilo, sigma_ciup) %>% 
    group_by(job.id) %>% 
    distinct() %>% 
    ungroup() %>% 
    mutate(mse_sigma = (sigma_est - true_sigma)^2)
  
  summary_res <- left_join(summary_res, summary_res_aux, by = c("job.id" = "job.id")) %>% 
    select(job.id, starts_with("RMSE"), starts_with("mse"), starts_with("cov"))
  return(summary_res)
}

## to extract legend
## https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graph
g_legend <- function(a.gplot) {
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

## prop of best (lowest) AIC/BIC
rereduce <- function(data, method = c("aic", "bic", "dev_expl"), model_name) {
  data %>% 
    select(job.id, aic, bic, dev_expl) %>% 
    unique() %>% 
    rename_with(.fn = ~paste0(., "_", model_name), .cols = c("aic", "bic", "dev_expl"))
}

## sum of best models (chosen according to best AIC/BIC/DEV_EXPL) in each
## replication across each shape and sigma
best_aic_models <- function(data) {
  data %>% 
    mutate(min_aic = apply(.[,c(4, 7, 10)], 1, function(x) names(x)[which.min(x)]),
           min_bic = apply(.[,c(5, 8, 11)], 1, function(x) names(x)[which.min(x)]),
           max_dev = apply(.[,c(6, 9, 12)], 1, function(x) names(x)[which.max(x)])) %>% 
    group_by(data_generation1, data_generation2) %>% 
    summarise(lowest_aic_PAMM        = sum(min_aic == "aic_PAMM_WCE"),
              lowest_aic_PAMM_RIDGE  = sum(min_aic == "aic_PAMM_WCE_RIDGE"),
              lowest_aic_PAMM_CONSTR = sum(min_aic == "aic_PAMM_WCE_CONSTR."),
              lowest_bic_PAMM        = sum(min_bic == "bic_PAMM_WCE"),
              lowest_bic_PAMM_RIDGE  = sum(min_bic == "bic_PAMM_WCE_RIDGE"),
              lowest_bic_PAMM_CONSTR = sum(min_bic == "bic_PAMM_WCE_CONSTR."),
              greatest_dev_PAMM        = sum(max_dev == "dev_expl_PAMM_WCE"),
              greatest_dev_PAMM_RIDGE  = sum(max_dev == "dev_expl_PAMM_WCE_RIDGE"),
              greatest_dev_PAMM_CONSTR = sum(max_dev == "dev_expl_PAMM_WCE_CONSTR."),
              .groups = "keep")
}
