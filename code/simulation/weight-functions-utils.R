## weight functions

# (a) exponential decay
hshape1 <- function(lag) 8.5/100*exp(-5*(lag/50))

# (b) bi-linear
hshape2 <- function(lag) ifelse((lag/100) <= 0.25, 0.08*(1 - (lag/100)/(25/100)), 0)

# (c) early peak
hshape3 <- function(lag) dnorm(lag/100, 0.04, 0.05)*0.01

# (d) inverted U
hshape4 <- function(lag) dnorm(lag/100, 0.2, 0.06)*0.08/6.5


