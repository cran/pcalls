\name{AmericanDigitalCalls}

\alias{AmericanDigitalCalls}

\title{Function that returns the price of an American Digital Call}

\description{Digital options, also called binary option, are options which pay a fixed payoff when the underlying stock price crosses the strike price. Thus, American Digital Options are automatically exercised as soon as they get in the money. In addition, American Digital options can be splitted in two categories: cash-or-nothing (which pays a fixed payoff in case of the underlying stock price ends up in the money) and asset-or-nothing (which pays the value of the underlying stock in case of the underlying stock price ends up in the money) options.}

\usage{AmericanDigitalCalls(s0, k, t, r, vol, call_type)}

\arguments{
  \item{s0}{stock price at time 0}
  \item{k}{strike price}
  \item{t}{time to maturity in years}
  \item{r}{annual interest rate}
  \item{vol}{annual volatility}
  \item{call_type}{"A":asset or "C":cash}
}

\details{No details}

\value{Price of the call}

\references{"Option Pricing Using Different Techniques" by Degiorgi Elia, Milan Federico, Zaramella Davide, Stoeva Valerija (2019)}

\author{Degiorgi Elia, Milan Federico, Zaramella Davide, Stoeva Valerija}

\section{Warning}{Be sure that the type of the call is "A" or "C". All input values must be stricly positive.}

\examples{AmericanDigitalCalls(10,11,1,0.05,0.2,"A") #  4.277183}
