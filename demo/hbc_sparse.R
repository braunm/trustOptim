

if(require(sparseHessianFD)) {

    set.seed(123)
    
    N <- 500
    k <- 8
    T <- 20
    
    ## simulate data
    x.mean <- rep(0,k)
    x.var <- rep(0.1,k)
    x.cov <- diag(x.var)
    x.cov[1,k-1] <- 0.8*sqrt(x.var[1]*x.var[k-1])
    x.cov[k-1,1] <- x.cov[1,k-1]
    
    mu <- rnorm(k,0,4)
    Omega <- diag(k)
    inv.Sigma <- rWishart(1,k+5,diag(k))[,,1]
    inv.Omega <- solve(Omega)

    X <- chol(x.cov) %*% matrix(rnorm(N*k),k,N) + x.mean
    B <- chol(x.cov) %*% matrix(rnorm(N*k),k,N) + mu
    XB <- colSums(X * B)
    log.p <- XB - log1p(exp(XB))
    Y <- sapply(log.p, function(q) return(rbinom(1,T,exp(q))))
        
    
    ## get reasonable starting values
    reg <- suppressWarnings(glm((Y/T)~t(X)-1,family=binomial))
    start.mean <- coefficients(reg)
    start.cov <- summary(reg)$cov.unscaled

    start <- chol(start.cov) %*% matrix(rnorm((N+1)*k),k,N+1) + start.mean

    hess.struct <- hbc.hess.struct(N, k)

    ## object from sparseHessianFD
    obj <- sparseHessianFD::new.sparse.hessian.obj(
        start, fn=hbc.f, gr=hbc.grad,
        hs=hess.struct,
        Y=Y, X=X, inv.Omega=inv.Omega,
        inv.Sigma=inv.Sigma, T=T
        )

    ## run optimizer
    opt <- trust.optim(start, fn=obj$fn,
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

} else {
    error("demo requires package sparseHessianFD, which is not available")
}







