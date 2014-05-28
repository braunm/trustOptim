#' @title Trust-region optimization
#' 
#' @description Nonlinear optimizers using trust regions, with methods
#'   optimized for sparse Hessians.
#' 
#' @details
#' Trust region algorithm for nonlinear optimization. In addition to
#' being more stable and robust than optim, this package includes 
#' methods that are scalable and efficient (in terms
#' of both speed and memory usage) when the Hessian is sparse.
#' 
#' @references
#' Nocedal, Jorge, and Stephen J Wright. 2006. Numerical Optimization. Second edition. Springer.
#' 
#' Steihaug, Trond. 1983. The Conjugate Gradient Method and Trust
#' Regions in Large Scale Optimization. SIAM Journal on Numerical
#' Analysis 20 (3): 626-637.
#'
#' @docType package
#' @name trustOptim
#' @import Matrix Rcpp RcppEigen
#' @useDynLib trustOptim
NULL 
