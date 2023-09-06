## weight functions

# (a) exponential decay
hshape1 <- function(lag) 4.5/100*exp(-1*(lag/10))

# (b) bi-linear
hshape2 <- function(lag) ifelse((lag/100) <= 0.25, 0.04*(1 - lag/25), 0)

# (c) early peak
hshape3 <- function(lag) dnorm(lag/100, 0.04, 0.06)*0.04/8

# (d) inverted U
hshape4 <- function(lag) dnorm(lag/100, 0.2, 0.06)*0.04/6.5

# (e) constant
hshape5 <- function(lag) ifelse(lag <= 20, 0.02, 0)

# (f) hat
hshape6 <- function(lag) {
  res <- ifelse(lag <= 19, lag/500 + (lag/500)^3,
                ifelse(lag <= 22, 19/500 + (19/500)^3,
                       ifelse(lag <= 27, 27/500 + (27/500)^2 - lag/500 - (lag/500)^2, 0)))
  return(res)
}