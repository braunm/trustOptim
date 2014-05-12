## callTrust.R --   Part of the trustOptim package for the R programming language.
## This file is part of trustOptim, a nonlinear optimization package
## for the R statistical programming platform.
##
## Copyright (C) 2012 Michael Braun
##
## This Source Code Form is subject to the license terms of the Mozilla
## Public License v. 2.0. If a copy of the MPL was not distributed
## with this file, you can obtain one at http://mozilla.org/MPL/2.0/.
## See the trustOptim LICENSE file for more information.

trust.optim <- function(x, fn, gr, hs=NULL, method=c("SR1","BFGS","Sparse"), control = list(), ...)
{

  if (is.null(method) || (match(method,c("SR1","BFGS","Sparse"),nomatch=0)==0)) {
    stop("Error in trust.optim:  mathod must be SR1, BFGS, or Sparse.")
  }
  
  if (!is.function(fn)) stop ("Error in trust.optim:  fn must be a function")
  if (!is.function(gr)) stop ("Error in trust.optim:  gr must be a function")
  
  
  fn1 <- function(x) fn(x,...)  ## currying the data in
  gr1 <- if (!is.null(gr)) function(x) gr(x,...)

    

## test fn and gr

  r1 <- fn1(x)
  if (!is.finite(r1)) stop("Error in trust.optim:  fn at starting values is not finite.")
  r1 <- gr1(x)
  if (any(!is.finite(r1))) stop("Error in trust.optim:  at least one element of gr is not finite.")
  if (length(r1)!=length(x)) stop("Error in trust.optim:  length of gradient does not match length of starting values.")

  if(method=="Sparse") {
    hs1 <- if (!is.null(hs)) function(x) hs(x,...)
    r1 <- hs1(x)
    if (class(r1)!="dgCMatrix") stop("Error in trust.optim:  hs function must return object of class dgCMatrix")
  }
  
    ## Defaults :

  con <- list(start.trust.radius=5.0,
              stop.trust.radius=sqrt(.Machine$double.eps),
              cg.tol=sqrt(.Machine$double.eps),
              prec=sqrt(.Machine$double.eps),
              report.freq=1L,
              report.level=2L,
              report.precision=6L,
              maxit=100L,
              contract.factor = 0.5,
              expand.factor = 3,
              contract.threshold = 0.25,
              expand.threshold.ap = 0.8,
              expand.threshold.radius = 0.8,
              function.scale.factor = as.numeric(1),   
              precond.refresh.freq=1L,
              preconditioner=0L,
              quasi.newton.method=0L,
              trust.iter=2000L
              )

    nmsC <- names(con)

    con[(namc <- names(control))] <- control


## check control parameter values here     

  if (!is.numeric(con$start.trust.radius) || con$start.trust.radius<=0 || !is.finite(con$start.trust.radius)) {
    stop("Error in trust.optim:  bad value for start.trust.radius.")
  }

  if (!is.numeric(con$stop.trust.radius) || (con$stop.trust.radius <= 0) || !is.finite(con$stop.trust.radius)) {
    stop("Error in trust.optim:  bad value for stop.trust.radius.")
  }

  if (!is.numeric(con$cg.tol) || con$cg.tol<=0 || !is.finite(con$cg.tol)) {
    stop("Error in trust.optim:  bad value for cg.tol.")
  }

  if (!is.numeric(con$prec) || con$prec<=0 || !is.finite(con$prec)) {
    stop("Error in trust.optim:  bad value for prec.")
  }

  if (!is.integer(con$report.freq)) {
    stop("Error in trust.optim:  report.freq must be an integer.")
  }

  if (!is.integer(con$report.level)) {
    stop("Error in trust.optim:  report.level must be an integer.")
  }

  if (!is.integer(con$report.precision)) {
    stop("Error in trust.optim:  report.precision must be an integer.")
  }

  if (!is.integer(con$maxit) || con$maxit<0) {
    stop("Error in trust.optim:  maxit must be an non-negative integer.")
  }

  if (!is.numeric(con$contract.threshold) || con$contract.threshold<0) {
    stop("Error in trust.optim:  contract.threshold must be an non-negative numeric.")
  }

  if (!is.numeric(con$expand.threshold.ap) || con$expand.threshold.ap<0) {
    stop("Error in trust.optim:  expand.threshold.ap must be an non-negative numeric.")
  }

  if (!is.numeric(con$expand.threshold.radius) || con$expand.threshold.radius<0) {
    stop("Error in trust.optim:  expand.threshold.radius must be an non-negative numeric.")
  }

  if (!is.numeric(con$expand.factor) || con$expand.factor<0) {
    stop("Error in trust.optim:  expand.factor must be an non-negative numeric.")
  }

 

  if (method=="Sparse") {
     
    con$quasi.newton.method <- 0L    
    res <- .Call("sparseTR", x, fn1, gr1, hs1, con)
    res$hessian <- Matrix::t(as(res$hessian,"symmetricMatrix"))
    
  }

  if (method=="SR1" || method=="BFGS") {
    
    if (!is.null(hs)) {
      warning("warning: Hessian function will be ignored for quasi-Newton (non-sparse) method.")
    }
    
    if (method=="SR1") {
      if (con$preconditioner==1) {
        warning("warning:  Cannot use Cholesky decomposition as preconditioner for SR1 method.  Using identity preconditioner instead.")
        con$preconditioner <- 0L
      }
      con$quasi.newton.method <- 1L
    } else {
      con$quasi.newton.method <- 2L
    }
    res <- .Call("quasiTR", x, fn1, gr1, con)    
  }
 
  return(res)
}

