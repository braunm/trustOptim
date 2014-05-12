## utils.R --   Part of the trustOptim package for the R programming language.
## This file is part of trustOptim, a nonlinear optimization package
## for the R statistical programming platform.
##
## Copyright (C) 2012 Michael Braun
##
## This Source Code Form is subject to the license terms of the Mozilla
## Public License v. 2.0. If a copy of the MPL was not distributed
## with this file, you can obtain one at http://mozilla.org/MPL/2.0/.
## See the trustOptim LICENSE file for more information.## utilities to support generalized direct sampling

vech <- function( M )
{
    if ( nrow(M)!=ncol(M))
        stop( "argument M is not a square numeric matrix" )
    return( t( t( M[!upper.tri(M)] ) ) )
}


inv.vech <- function( x ) {

  n <- length(x)
  R <- (sqrt(1+8*n)-1)/2

  if (R != as.integer(R)) {
    stop ("in function inv.vech:  vector will not fit in square matrix\n")
  }

  res <- matrix(0,R, R)
  res[!upper.tri(res)] <- x
  return(res)
}


logit <- function(p) {

  if (any(p<=0) || any(p>=1)) {
    stop (" in function logit:  all elements must be in open interval (0,1)")
  }
  
  res <- log(p) - log(1-p)
  return(res)
  
}

inv.logit <- function(x) {

  ## numerically stable inv.logit
  
  w.max <- x>=log(.Machine$double.xmax)
 
  res <- exp(x - log1p(exp(x)))
  res[w.max] <- 1
  return(res)

}

log_inv.logit <- function(x) {

  w.max <- x>=log(.Machine$double.xmax)
  w.min <- x<=log(.Machine$double.xmin)
  ww <- !(w.min | w.max)
  
  res <- x
  res[ww] <- x - log1p(exp(x))
  res[w.max] <- 0
  return(res)
  
}


