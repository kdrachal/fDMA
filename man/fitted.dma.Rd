\name{fitted.dma}
\alias{fitted.dma}
\alias{fitted}

\title{Extracts Fitted Values from \code{dma} Model.}

\description{The function extracts predictions made by the \code{\link{fDMA}} model.}

\usage{\method{fitted}{dma}(object, ...)}

\arguments{
\item{object}{an object of \code{dma} class}
\item{...}{not used}
}

\value{\code{\link[base]{vector}} of forecasted values
}

\examples{
wti <- crudeoil[-1,1]
drivers <- (lag(crudeoil[,-1],k=1))[-1,]
ld.wti <- (diff(log(wti)))[-1,]
ld.drivers <- drivers[-1,]
ld.drivers[,c(4,6)] <- (diff(drivers[,c(4,6)]))[-1,]
ld.drivers[,c(1:2,5,7)] <- (diff(log(drivers[,c(1:2,5,7)])))[-1,]
ld.drivers[,c(3,6)] <- ld.drivers[,c(3,6)]/100
\donttest{
m1 <- fDMA(y=ld.wti,x=ld.drivers,alpha=0.99,lambda=0.99,initvar=1,model="dma")
f <- fitted(object=m1)
}
}
