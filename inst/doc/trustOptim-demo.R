## ----, echo = FALSE------------------------------------------------------
knitr::opts_chunk$set(collapse = FALSE, comment = "#", message=FALSE)

## ------------------------------------------------------------------------
set.seed(123)
data(binary)
str(binary)
N <- length(binary$Y)
k <- NROW(binary$X)
nvars <- as.integer(N*k + k)
start <- rnorm(nvars) ## random starting values
priors <- list(inv.Sigma = rWishart(1,k+5,diag(k))[,,1],
               inv.Omega = diag(k))

## ------------------------------------------------------------------------

opt <- trust.optim(start, fn=binary.f,
                   gr = binary.grad,  
                   hs = binary.hess,
                   method = "Sparse",
                   control = list(
                       start.trust.radius=5,
                       stop.trust.radius = 1e-7,
                       prec=1e-7,
                       report.precision=1L,
                       maxit=500L,
                       preconditioner=1L,
                       function.scale.factor=-1
                   ),
                   data=binary, priors=priors
                   )

