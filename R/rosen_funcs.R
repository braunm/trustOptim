#' @title Multivariate Rosenbrock function, derivative, Hessian
#'
#' @description Objective function, gradient and hessian for demos.
#' 
#' @param V Vector of length 2N, ordered \eqn{(x_1,y_1,x_2,y_2,\mathellipsis, x_N,y_N)}
#'
#' @details
#'   The objective function is:
#' \deqn{
#'  f(x_{1:N},y_{1:N})=\sum_{i=1}^N
#' \left[100\left(x^2_i-y_i\right)^2+\left(x_i-1\right)^2\right]
#' }
#' These functions are provided for the examples and demos in the
#' \pkg{trustOptim} package.
#'
#' @return \describe{
#' \item{f.rosenbrock}{a scalar value of the objective function}
#' \item{df.rosenbrock}{a numeric vector for the gradient of the function.}
#' \item{hess.rosenbrock}{a symmetric matrix of class \code{dgCMatrix} (defined
#'     in the \pkg{Matrix} package) for the Hessian of the function.}
#' }
#' @rdname rosen
#' @export
f.rosenbrock <- function(V) {

    N <- length(V)/2
    x <- V[seq(1,2*N-1,by=2)]
    y <- V[seq(2,2*N,by=2)]
    return(sum(100*(x^2-y)^2+(x-1)^2))
}

#' @rdname rosen
#' @export
df.rosenbrock <- function(V) {
    N <- length(V)/2
    x <- V[seq(1,2*N-1,by=2)]
    y <- V[seq(2,2*N,by=2)]

    t <- x^2-y
    dxi <- 400*t*x+2*(x-1)
    dyi <- -200*t
    return(as.vector(rbind(dxi,dyi)))
 }

#' @rdname rosen
#' @export
hess.rosenbrock <- function(V) {

    N <- length(V)/2
    x <- V[seq(1,2*N-1,by=2)]
    y <- V[seq(2,2*N,by=2)]
    d0 <- rep(200,N*2)
    d0[seq(1,(2*N-1),by=2)] <- 1200*x^2-400*y+2
    d1 <- rep(0,2*N-1)
    d1[seq(1,(2*N-1),by=2)] <- -400*x

    H <- bandSparse(2*N,
                    k=c(-1,0,1),
                    diagonals=list(d1,d0,d1),
                    symmetric=FALSE,
                    giveCsparse=TRUE)
    return(drop0(H))
}

