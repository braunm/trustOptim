
#' #' @title Vech operator on a matrix.
#' @description Vector of elements of lower triangle of matrix, including diagonal,
#'   in column-wise order
#' @param M A matrix.
#' @return A vector of conforming length
#' @details If M is a k x k matrix, then x is a vector of length k(k+1)/2.
#' @export
#' @keywords internal
vech <- function( M )
{
    if ( nrow(M)!=ncol(M))
        stop( "argument M is not a square numeric matrix" )
    return( t( t( M[!upper.tri(M)] ) ) )
}




#' @title Inverse vech operator on a vector
#' @description Returns a lower triangular matrix from a vector, filled columnwise.
#' @param x a vector of conforming length.
#' @details x must be a vector of length k(k+1)/2, where k is the number of rows (and columns)
#'   of the result.
#' @return A k x k lower triangular matrix.
#' @export
#' @keywords internal
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



