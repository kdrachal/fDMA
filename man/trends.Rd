\name{trends}
\alias{trends}

\title{Google Trends for Crude Oil Data.}

\description{Google Trends for Crude Oil Data.}

\usage{data(trends)}

\format{\code{trends} is \code{\link[xts]{xts}} object such that
\itemize{
\item \code{trends$prod} -- Google Trends for "oil production" 
\item \code{trends$cons} -- Google Trends for "oil consumption"
\item \code{trends$econ_act} -- Google Trends for "economic activity"
\item \code{trends$r} -- Google Trends for "interest rate"
\item \code{trends$stocks} -- Google Trends for "stock markets"
\item \code{trends$risk} -- Google Trends for "market stress"
\item \code{trends$ex_rate} -- Google Trends for "exchange rate"
}
}

\details{The data are in monthly frequency. They cover the period between Jan, 2004 and Oct, 2024.
}

\source{The data are provided by Google. 

\url{https://trends.google.com/trends}
}

\examples{
data(trends)
gtrends <- trends/100
data(crudeoil)
wti <- crudeoil[-1,1]
drivers <- (lag(crudeoil[,-1],k=1))[-1,]
ld.wti <- (diff(log(wti)))[-1,]
ld.drivers <- drivers[-1,]
ld.drivers[,c(4,6)] <- (diff(drivers[,c(4,6)]))[-1,]
ld.drivers[,c(1:2,5,7)] <- (diff(log(drivers[,c(1:2,5,7)])))[-1,]
ld.drivers[,c(3,6)] <- ld.drivers[,c(3,6)]/100
ld.wti <- ld.wti['2004-01-01-/']
ld.drivers <- ld.drivers['2004-01-01-/']
\donttest{
xx <- ld.drivers
m <- fDMA(y=ld.wti,x=xx,alpha=0.99,lambda=0.99,initvar=1,model="dma",gprob=gtrends,omega=0.5)
}
}
