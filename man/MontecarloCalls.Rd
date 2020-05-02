\name{MontecarloCalls}

\alias{MontecarloCalls}

\title{Function that prices a Call via Montecarlo simulation}

\description{Montecarlo is a method used to price options. It computes the expected value of the price with respect to an underlying probability distribution which is assumed to be a Gaussian stochastic process described by a geometric Brownian motion.}

\usage{MontecarloCalls(s0, k, t, r, vol, n)}

\arguments{
  \item{s0}{stock price at time 0}
  \item{k}{strike price}
  \item{t}{time to maturity in years}
  \item{r}{annual interest rate}
  \item{vol}{annual volatility}
  \item{n}{number of simulations}
}

\details{No details}

\value{Price of the call}

\references{"Option Pricing Using Different Techniques" by Degiorgi Elia, Milan Federico, Zaramella Davide, Stoeva Valerija (2019)}

\author{Degiorgi Elia, Milan Federico, Zaramella Davide, Stoeva Valerija}

\examples{MontecarloCalls(10,11,1,0.05,0.2,100) #  0.6164035}
