
#' @title Logit (log odds) of a probability
#' @description Returns the logit of x.
#' @param p a scalar, vector or matrix, where all elements are between 0 and 1
#' @return result = log(p/(1-p))
#' @export
#' @keywords internal
logit <- function(p) {

  if (any(p<=0) || any(p>=1)) {
    stop (" in function logit:  all elements must be in open interval (0,1)")
  }
  
  res <- log(p) - log(1-p)
  return(res)
  
}

#' @title Inverse logit
#' @description Returns the inverse logit of elements in x.
#' @param x a scalar, vector or matrix
#' @return result = exp(x) / (1+exp(x))
#' @export
#' @keywords internal

inv.logit <- function(x) {
  
  w.max <- x>=log(.Machine$double.xmax)
  res <- exp(x - log1p(exp(x)))
  res[w.max] <- 1
  return(res)

}

#' @title Log Inverse logit
#' @description Returns the natural log of the inverse logit of elements in x.
#' @param x a scalar, vector or matrix
#' @return result = log(exp(x) / (1+exp(x)))
#' @details Intended to be a numerically stable alternative to just doing
#'   log(inv.logit(x)).  Should be less sensitive to overflow and underflow with
#'   very large or very small x.
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


