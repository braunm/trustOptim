

demo.get.f <- function(pars, Y, X, inv.Omega, inv.Sigma,T) {

  ## log likelihood of hierarchical binary choice example
  k <- NROW(X)
  N <- NCOL(X)
  
  beta <- matrix(pars[1:(N*k)], k, N, byrow=FALSE)
  mu <- pars[(N*k+1):(N*k+k)]

  bx <- colSums(X * beta)
  
  log.p <- bx - log1p(exp(bx))
  log.p1 <- -log1p(exp(bx))
  
  data.LL <- sum(Y*log.p + (T-Y)*log.p1)
  
  Bmu <- apply(beta, 2, "-", mu)
  
  prior <- -0.5 * sum(diag(tcrossprod(Bmu) %*% inv.Sigma))
  hyp <- -0.5 * t(mu) %*% inv.Omega %*% mu
  res <- data.LL + prior + hyp
  return(-as.numeric(res)) ## return negative value
  
}

.demo.dlog.f.db <- function(pars, Y, X, inv.Omega, inv.Sigma, T) {

  k <- NROW(X)
  N <- NCOL(X)
  
  beta <- matrix(pars[1:(N*k)], k, N, byrow=FALSE)
  mu <- pars[(N*k+1):length(pars)]
  bx <- colSums(X * beta)

  p <- exp(bx)/(1+exp(bx))

  tmp <- Y - T*p

  dLL.db <- apply(X,1,"*",tmp)
    
  Bmu <- apply(beta, 2, "-", mu)
  dprior <- -inv.Sigma %*% Bmu
  
  res <- t(dLL.db) + dprior

  return(as.vector(res))
 
}

.demo.dlog.f.dmu <- function(pars, Y, X, inv.Omega, inv.Sigma) {

  k <- NROW(X)
  N <- NCOL(X)
  
  beta <- matrix(pars[1:(N*k)], k, N, byrow=FALSE)
  mu <- pars[(N*k+1):length(pars)]
  Bmu <- apply(beta, 2, "-", mu)

  res <- inv.Sigma %*% (rowSums(Bmu)) -  inv.Omega %*% mu
  return(res)
}

demo.get.grad <- function(pars, Y, X, inv.Omega, inv.Sigma,T) {

  ## returns gradient of the log posterior density
  ## of the hierarchical binary choice example
  
  q1 <- .demo.dlog.f.db(pars, Y, X, inv.Omega, inv.Sigma, T)
  q2 <- .demo.dlog.f.dmu(pars, Y, X, inv.Omega, inv.Sigma)
  res <- c(q1, q2)
  if(any(!is.finite(res))) browser()
  return(-res) ## return negative gradient
}


demo.get.f.dense <- function(pars, Y, X, inv.Omega, T) {

  ## log likelihood of hierarchical binary choice example
  

  bx <- colSums(X * pars)
  
  log.p <- bx - log1p(exp(bx))
  log.p1 <- -log1p(exp(bx))
  
  data.LL <- sum(Y*log.p + (T-Y)*log.p1)
  
  prior <- -0.5 * t(pars) %*% inv.Omega %*% pars
  res <- data.LL + prior
##  if(!is.finite(res)) browser()
  return(-as.numeric(res)) ## return negative value
  
}


demo.get.grad.dense <- function(pars, Y, X, inv.Omega, T) {
  
  ## log likelihood of hierarchical binary choice example
  
  
  bx <- colSums(X * pars)
  p <- exp(bx)/(1+exp(bx))
  
  tmp <- Y - T*p
  dLL.db <- colSums(apply(X,1,"*",tmp))
  dprior.db <- -pars %*% inv.Omega
  
  res <- dLL.db + dprior.db
  return(-as.vector(res))
  
}



demo.get.hess.struct <- function(N, k) {

  ## For the hierarchical binary choice example,
  ## returns a list of row and column indices of the
  ## non-zero elements of the lower triangle of
  ## the Hessian.  Suitable to be passed to the
  ## get.sparse.hessian.obj function in the
  ## sparseHessianFD package.
  
  B1 <- kronecker(Diagonal(N),Matrix(TRUE,k,k))
  B2 <- Matrix(TRUE,k,N*k)
  B3 <- Matrix(TRUE,k,k)
  H <- cBind(rBind(B1,B2),rBind(Matrix::t(B2),B3))
  res <- Matrix.to.Coord(H)
  return(res)
  
}


demo.trust.func <- function(pars, obj, ...) {

  ## takes object generated from sparseHessianFD package
  ## and returns a function that can be passed to the trust
  ## function in the trust package.
  
  res <- obj$all(pars)
  res$hs <- as(res$hs,"matrix")
  names(res) <- c("value","gradient","hessian")
  return(res)

}
