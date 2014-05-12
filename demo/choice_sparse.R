## Runs the hierarchical binary choice example

library(Matrix)
library(mvtnorm)
library(trustOptim)
library(plyr)
library(sparseHessianFD)


N <- 500  ## number of heterogeneous units
k <- 8  ## number of covariates.  Must be k>=3 for simulation to run
T <- 20  ## number of "purchase opportunities per unit


## Simulate data and set priors

x.mean <- rep(0,k-1)
x.var <- rep(0.1,k-1)
x.cov <- diag(x.var)
x.cov[1,k-2] <- 0.8*sqrt(x.var[1]*x.var[k-2])
x.cov[k-2,1] <- x.cov[1,k-2]


mu <- rnorm(k,0,4)
Omega <- diag(k)
inv.Sigma <- rWishart(1,k+5,diag(k))[,,1]
inv.Omega <- solve(Omega)

X <- rbind(1,t(rmvnorm(N, mean=x.mean, sigma=x.cov))) ## k x N
B <- t(rmvnorm(N, mean=mu, sigma=diag(k))) ## k x N
XB <- colSums(X * B)
log.p <- XB - log1p(exp(XB))
Y <- laply(log.p, function(q) return(rbinom(1,T,exp(q))))

## get reasonable starting values
reg <- glm((Y/T)~t(X)-1,family=binomial)
start.mean <- coefficients(reg)
start.cov <- summary(reg)$cov.unscaled
start <- as.vector(t(rmvnorm(N+1,mean=start.mean,sigma=start.cov)))

hess.struct <- demo.get.hess.struct(N, k)


## Setting up function to compute Hessian using sparseHessianFD package.
obj <- new.sparse.hessian.obj(start, fn=demo.get.f, gr=demo.get.grad,
                              hs=hess.struct, Y=Y, X=X,
                              inv.Omega=inv.Omega,
                              inv.Sigma=inv.Sigma, T=T)


td <- system.time(opt <- trust.optim(start, fn=obj$fn,
                                     gr = obj$gr,
                                     hs = obj$hessian,
                                     method = "Sparse",
                                     control = list(
                                       start.trust.radius=5,
                                       stop.trust.radius = 1e-7,
                                       prec=1e-7,
                                       report.freq=1L,
                                       report.level=4L,
                                       report.precision=1L,
                                       maxit=500L,
                                       preconditioner=1L
                                       ) 
                                     )
                  )
print(td)
