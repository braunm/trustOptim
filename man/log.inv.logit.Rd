\name{log_inv.logit}
\alias{log_inv.logit}
\title{log of inverse logit function}
\description{
Returns the log inverse logit of x.
	}
\usage{
log_inv.logit(x)
}

\arguments{
	\item{x}{a scalar, vector or matrix.}
}
\value{
result = log[exp(x) / (1+exp(x))]
}
\details{
Intended to be a numerically stable alternative to just doing log(inv.logit(x)).  Should be less sensitive to overflow and underflow with very large or very small x.
}






 


