\name{BinaryTreeCalls}

\alias{BinaryTreeCalls}

\title{Function that prices a Call via Binary Tree}

\description{The Binomial Option Pricing Model is a method which uses an iterative procedure to evaluate options. Based on a discrete time interval and a multi-period approach, the model evaluates each time the option generating an upward or downward movement of the underlying price. In each node the price of the option can take only two values: the first one corresponds to the probability that the price of the option goes up whereas the second one corresponds to the probability that the price drops.}

\usage{BinaryTreeCalls(s0, k, r, vol, deltaT, nsteps)}

\arguments{
  \item{s0}{stock price at time 0}
  \item{k}{strike price}
  \item{r}{annual interest rate}
  \item{vol}{annual volatility}
  \item{deltaT}{time variation in years}
  \item{nsteps}{number of steps}
}
\details{No details}

\value{Price of the call}

\references{"Option Pricing Using Different Techniques" by Degiorgi Elia, Milan Federico, Zaramella Davide, Stoeva Valerija (2019)}

\author{Degiorgi Elia, Milan Federico, Zaramella Davide, Stoeva Valerija}

\section{Warning}{All input values must be stricly positive.}

\examples{BinaryTreeCalls(10,11,0.05,0.2,0.01,100) #  0.6053225 }


