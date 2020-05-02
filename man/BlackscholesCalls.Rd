\name{BlackscholesCalls}

\alias{BlackscholesCalls}

\title{Function that prices a Call via Black-Scholes formula}

\description{Black-Scholes is a model used to price Vanilla European Options assuming that the market is free from arbitrage and the underlying asset price follows a geometric Brownian motion. In other words, it assumes that the underlying stock price follows a random walk and it partially satisfies the efficient market hypothesis.}

\usage{BlackscholesCalls(s0, k, t, r, vol)}

\arguments{
  \item{s0}{stock price at time 0}
  \item{k}{strike price}
  \item{t}{time to maturity in years}
  \item{r}{annual interest rate}
  \item{vol}{annual volatility}
}

\details{No details}

\value{Price of the call}

\references{"Option Pricing Using Different Techniques" by Degiorgi Elia, Milan Federico, Zaramella Davide, Stoeva Valerija (2019)}

\author{Degiorgi Elia, Milan Federico, Zaramella Davide, Stoeva Valerija}

\section{Warning}{All input values must be stricly positive.}

\examples{BlackscholesCalls(10,11,1,0.05,0.2) #  0.6040088}
