##! Multivariate Rosenbrock function
##! V is a 2N vector, ordered (x_1,y_1,x_2,y_2,...,x_N,y_N)

N <- 5
start <- as.vector(rnorm(2*N,-1,3))
res <- trust.optim(start,
                   fn=f.rosenbrock,
                   gr=df.rosenbrock,
                   method="SR1",
                   control=list(report.freq=1L,
                       report.level=3L,
                       report.precision=2L,
                       maxit=50000L
                       )
                   )
print(res)



