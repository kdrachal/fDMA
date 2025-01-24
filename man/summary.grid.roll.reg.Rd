\name{summary.grid.roll.reg}
\alias{summary.grid.roll.reg}

\title{Summarizes Outcomes from \code{grid.roll.reg} Objects.}

\description{The function summarizes outcomes obtained from \code{\link{grid.roll.reg}}.}

\usage{\method{summary}{grid.roll.reg}(object, ...)}

\arguments{
\item{object}{an object of \code{grid.roll.reg} class}
\item{...}{not used}
}

\value{Called for printing.
}

\details{
The function produces the outcomes as \code{\link{print.grid.roll.reg}}.

Additionally, it finds the model minimizing Root Mean Squared Error (RMSE) and minimizing Mean Absolute Error (MAE). 
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
grw <- c(50,100,150)
g <- grid.roll.reg(y=ld.wti,x=ld.drivers,grid.window=grw)

summary(g)
}
}

