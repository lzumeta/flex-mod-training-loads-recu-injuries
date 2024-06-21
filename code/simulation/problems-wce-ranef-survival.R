
sim_wce_ranef_ped <- function(
    data,
    job) {
  
  X      <- data$X
  nl_rep <- data$nl_rep
  
  sdf      <- data.frame(id = X$id, rate = exp(X$eta), t = X$time)
  ## Generate replicates of row depending on number of events that each
  ## individual is at risk of
  sdf      <- sdf[rep(1:nrow(sdf), nl_rep), ] 
  ## Add enum, the identifier of subject recurrence, and arrange sdf per
  ## id and enum
  sdf      <- sdf %>% 
    mutate(enum = row_number(), .by = c(id, t)) %>% 
    arrange(id, enum)
  ## split sdf per id and enum
  rt.l     <- split(sdf[,-1], ~ sdf$id + sdf$enum, drop = TRUE)  
  rt.l     <- lapply(rt.l, function(elem) {elem$enum = NULL; return(as.list(elem))})
  ## random part (draw t_{i_{l}j} survival times)
  new_time <- vapply(rt.l, do.call, what=msm::rpexp, numeric(1)) 
  
  ## reorder this vector in a convenient way (because split() messed it up) 
  new_time <- new_time[order(as.numeric(names(new_time)))] 
  sv_df <- select(sdf, id, enum) %>% 
    unique() %>%                    ## quit duplicates because of the X part
    mutate(surv = new_time) %>% 
    arrange(id, surv) %>%           ## sort surv from lowest to largest within each individual
    group_by(id) %>% 
    mutate(enum = row_number()) %>% ## after sorting 'surv' put proper enum's per individual
    ungroup()
  
  # Merge X and sv_df 
  ## Generate replicates of row depending on number of events that each
  ## individual is at risk of
  X <- X[rep(1:nrow(X), nl_rep), ]
  X <- X %>% 
    mutate(enum = row_number(), .by = c(id, time)) %>% 
    arrange(id, enum) 
  sv_df <- sv_df %>% 
    mutate(surv_lagged = lag(surv, default = 0), .by = c(id))
  ## Important part (tstart >= surv_lagged part applies for tstart that
  ## corresponds to a recurrence, for which we force this value to start to
  ## count from the last observed time on)
  X <- inner_join(X, sv_df, 
                  by = join_by(id == id, enum == enum, tstart >= surv_lagged),
                  multiple = "all") 
  
  X            <- X[X$time >= 0, ]
  X$offset     <- 0
  X$ped_status <- 0
  X                     <- X[X$tstart < X$surv,]
  ## ped_status and offset
  unique_id             <- paste0(X$id, ".", X$enum)
  last_id               <- cumsum(rle(unique_id)$lengths)  
  X$ped_status[last_id] <- 1*(X$surv[last_id] < 40)
  X$offset[last_id]     <- log(X$surv[last_id] - X$tstart[last_id])
  ## edit tz_df: 
  ## each element i,j in tz_df if != 0 means that the Z[i,j] value will affect
  ## at tend[i] and the value tz_df[i,j] represents the time elapsed since
  ## tend[i] i.e. the lag, the t-tz value.
  X$tz_df <- (X$time_df - X$tz_df)*X$LL     
  X$id    <- factor(X$id)                   ## for s(id, bs = "re")
  
  return(X)
  
}


## Check tz_df and Z*LL matrices
# image(1:80, 1:30, t(X$tz_df[30:1,]), yaxt = "n", main = "tz_df matrix for ID = 1")
# axis(side = 2, at = seq(0,30, by = 5), labels = seq(30, 0, by = -5))
# image(1:80, 1:30, t(X$Z[30:1,]*X$LL[30:1,]), yaxt = "n", main = "Z matrix for ID = 1")
# axis(side = 2, at = seq(0,30, by = 5), labels = seq(30, 0, by = -5))
# X$tend[1:30]
# X$time_df[1:30,1]
# 
# X_splitted <- split(X, X$id)
# for (i in 1:500) {
#   A <- X_splitted[[i]]
#   image(1:80, 1:nrow(A), t(A$tz_df[nrow(A):1,]), yaxt = "n", main = paste0("tz_df matrix for ID = ", i))
#   axis(side = 2, at = seq(0, nrow(A), by = 5), labels = seq(nrow(A), 0, by = -5))
#   image(1:80, 1:nrow(A), t(A$Z[nrow(A):1, ] *A$LL[nrow(A):1,]), yaxt = "n", main = paste0("Z matrix for ID = ", i))
#   axis(side = 2, at = seq(0, nrow(A), by = 5), labels = seq(nrow(A), 0, by = -5))
#   DAAG::pause()
# }
