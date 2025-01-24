\name{coef.dma}
\alias{coef.dma}
\alias{coef}

\title{Extracts Averaged Coefficients from \code{dma} Model.}

\description{The function extracts the expected values of regression coefficients from the \code{\link{fDMA}} model.}

\usage{\method{coef}{dma}(object, ...)}

\arguments{
\item{object}{an object of \code{dma} class}
\item{...}{not used}
}

\value{\code{\link[base]{matrix}} of expected values of regression coefficients
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
c <- coef(object=m1)
}
}
