\name{summary.tvp}
\alias{summary.tvp}

\title{Summarizes Outcomes from \code{tvp} Object.}

\description{The function summarizes selected outcomes obtained from \code{\link{tvp}}.}

\usage{\method{summary}{tvp}(object, ...)}

\arguments{
\item{object}{an object of \code{tvp} class}
\item{...}{not used}
}

\value{Called for printing.
}

\details{
The function produces the outcomes as \code{\link{tvp}}.
}

\examples{
wti <- crudeoil[-1,1]
drivers <- (lag(crudeoil[,-1],k=1))[-1,]
ld.wti <- (diff(log(wti)))[-1,]
ld.drivers <- drivers[-1,]
ld.drivers[,c(4,6)] <- (diff(drivers[,c(4,6)]))[-1,]
ld.drivers[,c(1:2,5,7)] <- (diff(log(drivers[,c(1:2,5,7)])))[-1,]
ld.drivers[,c(3,6)] <- ld.drivers[,c(3,6)]/100
tvp <- tvp(y=ld.wti,x=ld.drivers,V=1,lambda=0.99)
summary(tvp)
}
