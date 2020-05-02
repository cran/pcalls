\name{MontecarloAntitheticCalls}

\alias{MontecarloAntitheticCalls}

\title{Function that prices a Call via Montecarlo simulation using antithetic variates}

\description{The Antithetic Variates is a method which decreases the approximation error by reducing the variance of the simulation result.}

\usage{MontecarloAntitheticCalls(s0, k, t, r, vol, n)}

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

\examples{MontecarloAntitheticCalls(10,11,1,0.05,0.2,100) #  0.5749907}

