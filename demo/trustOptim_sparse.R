##! Multivariate Rosenbrock function
##! V is a 2N vector, ordered (x_1,y_1,x_2,y_2,...,x_N,y_N)

N <- 4
start <- as.vector(rnorm(2*N,-1,3))
opt <- trust.optim(start,
                   fn=f.rosenbrock,
                   gr=df.rosenbrock,
                   hs=hess.rosenbrock,
                   method="Sparse",
                   control=list(
                       report.freq=1L,
                       report.level=3L,
                       report.precision=2L,
                       maxit=1000L,
                       preconditioner=2L
                       )
                   )
print(opt)


