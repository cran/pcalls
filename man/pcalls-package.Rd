\name{pcalls-package}

\alias{pcalls-package}

\alias{pcalls}

\docType{package}

\title{
\packageTitle{pcalls}
}

\description{
\packageDescription{pcalls}
}

\author{
\packageAuthor{pcalls}

Maintainer: \packageMaintainer{pcalls}
}

\references{"Option Pricing Using Different Techniques" by Degiorgi Elia, Milan Federico, Zaramella Davide, Stoeva Valerija (2019)}

\keyword{ package }

\examples{
MontecarloCalls(10,11,1,0.05,0.2,100)
MontecarloAntitheticCalls(10,11,1,0.05,0.2,100)
BlackscholesCalls(10,11,1,0.05,0.2)
AmericanDigitalCalls(10,11,1,0.05,0.2,"A")
AmericanDigitalCalls(10,11,1,0.05,0.2,"C")
}
