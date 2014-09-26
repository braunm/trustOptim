##! Multivariate Rosenbrock function
##! V is a 2N vector, ordered (x_1,y_1,x_2,y_2,...,x_N,y_N)

N <- 3
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

cat("Number of variables: ",2*N,"\n")
cat("Starting values:\n")
print(start)
cat("Solution at minimum:\n")
print(opt$solution)
cat("Function value at minimum:\n")
print(opt$fval)
cat("Gradient at minimum:\n")
print(opt$gr)
cat("Hessian at minimum:\n")
print(opt$hessian)



