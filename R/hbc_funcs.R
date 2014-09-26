#' @title Functions for hierarchical binary choice example
#'
#' @description Functions that define the objective functions, gradients
#' and Hessian structures for the hierarchical binary choice example.
#'
#'
#' @param pars input parameters.
#' @param Y,X data
#' @param inv.Sigma,inv.Omega prior parameters
#' @param T number of observations per unit
#' @param N number of individual units
#' @param k length of individual parameter vector
#' @param ... Additional arguments
#'
#' @details
#'   These are functions that define the objective functions and gradients for the 
#'   demo examples in the vignette.
#'
#' @references
#' Braun, Michael.  2014.  trustOptim:  An R Package for Trust Region
#' Optimization with Sparse Hessians. Journal of Statistical Software 60(4),
#' 1-16. www.jstatsoft.org/v60/i04/.
#'
#' @rdname demo_funcs_hbc 
#' @export
hbc.f <- function(pars, Y, X, inv.Omega, inv.Sigma,T) {

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


#' @rdname demo_funcs_hbc 
#' @export
hbc.grad <- function(pars, Y, X, inv.Omega, inv.Sigma,T) {

  ## returns gradient of the log posterior density
  ## of the hierarchical binary choice example
  
  q1 <- hbc.dlog.f.db(pars, Y, X, inv.Omega, inv.Sigma, T)
  q2 <- hbc.dlog.f.dmu(pars, Y, X, inv.Omega, inv.Sigma)
  res <- c(q1, q2)
  return(-res) ## return negative gradient
}

#' @rdname demo_funcs_hbc 
#' @export
hbc.f.dense <- function(pars, Y, X, inv.Omega, T) {

  ## log likelihood of hierarchical binary choice example
  

  bx <- colSums(X * pars)
  
  log.p <- bx - log1p(exp(bx))
  log.p1 <- -log1p(exp(bx))
  
  data.LL <- sum(Y*log.p + (T-Y)*log.p1)
  
  prior <- -0.5 * t(pars) %*% inv.Omega %*% pars
  res <- data.LL + prior
  return(-as.numeric(res)) ## return negative value
  
}

#' @rdname demo_funcs_hbc 
#' @export
hbc.grad.dense <- function(pars, Y, X, inv.Omega, T) {
  
  ## log likelihood of hierarchical binary choice example
  
  
  bx <- colSums(X * pars)
  p <- exp(bx)/(1+exp(bx))
  
  tmp <- Y - T*p
  dLL.db <- colSums(apply(X,1,"*",tmp))
  dprior.db <- -pars %*% inv.Omega
  
  res <- dLL.db + dprior.db
  return(-as.vector(res))
  
}


#' @rdname demo_funcs_hbc
#' @export
hbc.hess.struct <- function(N, k) {

  ## For the hierarchical binary choice example,
  ## returns a list of row and column indices of the
  ## non-zero elements of the lower triangle of
  ## the Hessian.  Suitable to be passed to the
  ## get.sparse.hessian.obj function in the
  ## \pkg{sparseHessianFD} package.
  
  B1 <- kronecker(Diagonal(N),Matrix(TRUE,k,k))
  B2 <- Matrix(TRUE,k,N*k)
  B3 <- Matrix(TRUE,k,k)
  H <- cBind(rBind(B1,B2),rBind(Matrix::t(B2),B3))
  res <- sparseHessianFD::Matrix.to.Coord(H)
  return(res)
  
}

hbc.dlog.f.db <- function(pars, Y, X, inv.Omega, inv.Sigma, T) {

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

hbc.dlog.f.dmu <- function(pars, Y, X, inv.Omega, inv.Sigma) {

  k <- NROW(X)
  N <- NCOL(X)
  
  beta <- matrix(pars[1:(N*k)], k, N, byrow=FALSE)
  mu <- pars[(N*k+1):length(pars)]
  Bmu <- apply(beta, 2, "-", mu)

  res <- inv.Sigma %*% (rowSums(Bmu)) -  inv.Omega %*% mu
  return(res)
}
