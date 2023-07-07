## weight functions

# (a) exponential decay
hshape1 <- function(lag) 5/100*exp(-5*(lag/100))

# (b) bi-linear
hshape2 <- function(lag) ifelse((lag/100) <= 0.2, 0.04*(1 - (lag/100)/(20/100)), 0)

# (c) early peak
hshape3 <- function(lag) dnorm(lag/100, 0.08, 0.05)*0.04/8

# (d) inverted U
hshape4 <- function(lag) dnorm(lag/100, 0.2, 0.06)*0.04/6.5

# (e) constant
hshape5 <- function(lag) ifelse(lag <= 20, 0.02, 0)

# (f) hat
hshape6 <- function(lag) {
  res <- ifelse(lag <= 19, lag/365 + (lag/365)^3,
                ifelse(lag <= 22, 19/365 + (19/365)^3,
                       ifelse(lag <= 27, 27/365 + (27/365)^2 - lag/365 - (lag/365)^2, 0)))
  return(res)
}