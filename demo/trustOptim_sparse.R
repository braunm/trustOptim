##! Multivariate Rosenbrock function
##! V is a 2N vector, ordered (x_1,y_1,x_2,y_2,...,x_N,y_N)

control.list <- list(start.trust.radius=5,
                             stop.trust.radius = 1e-4,
                             prec=1e-7,
                             cg.tol=1e-5,
                             report.freq=2200L,
                             report.level=6L,
                             report.precision=2L,
                             maxit=2000000L,
                             preconditioner=2L, 
                             trust.iter=10000L
                             )

N <- 5
start <- as.vector(rnorm(2*N,-1,3))
r.trust <- trust.optim(start,
                       fn=f.rosenbrock,
                       gr=df.rosenbrock,
                       hs=hess.rosenbrock,
                       method="Sparse",
                           control=control.list
                       )



