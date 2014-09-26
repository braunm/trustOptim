
#' @title Logit (log odds) of a probability
#' @description Returns the logit, inverse logit, or log inverse logit of x.
#' @param p a scalar, vector or matrix, where all elements are between 0 and 1
#' @param x a scalar, vector or matrix
#' @return \describe{
#' \item{logit}{result = log(p/(1-p))}
#' \item{inv.logit}{result = exp(x) / (1+exp(x))}
#' \item{log_inv.logit}{result = x - log1p(exp(x))}
#' }
#' @rdname logit
#' @export
#' @keywords internal
logit <- function(p) {

  if (any(p<=0) || any(p>=1)) {
    stop (" in function logit:  all elements must be in open interval (0,1)")
  }
  
  res <- log(p) - log(1-p)
  return(res)
  
}

#' @rdname logit
#' @export
#' @keywords internal
inv.logit <- function(x) {
  
  w.max <- x>=log(.Machine$double.xmax)
  res <- exp(x - log1p(exp(x)))
  res[w.max] <- 1
  return(res)

}

#' @details log_inv.logit is a (often) numerically stable alternative
#' to log(inv.logit(x)).  It should be less sensitive to overflow and
#' underflow with very large or very small x.
#' @rdname logit
#' @export
#' @keywords internal
log_inv.logit <- function(x) {

  w.max <- x>=log(.Machine$double.xmax)
  w.min <- x<=log(.Machine$double.xmin)
  ww <- !(w.min | w.max)
  
  res <- x
  res[ww] <- x - log1p(exp(x))
  res[w.max] <- 0
  return(res)
  
}


