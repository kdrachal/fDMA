\name{crudeoil}
\alias{crudeoil}

\title{Crude Oil Data.}

\description{Selected data from oil market.}

\usage{data(crudeoil)}

\format{\code{crudeoil} is \code{\link[xts]{xts}} object such that
\itemize{
\item \code{crudeoil$p_oil} -- average spot price of crude oil (Brent, Dubai and WTI) in USD per barrel
\item \code{crudeoil$prod} -- U.S. field production of crude oil in thousand barrels
\item \code{crudeoil$cons} -- U.S. product supplied of crude oil and petroleum products in thousand barrels 
\item \code{crudeoil$econ_act} -- Index of Global Real Economic Activity
\item \code{crudeoil$r} -- U.S. 3-month treasury bill secondary market rate in \%
\item \code{crudeoil$stocks} -- U.S. share prices index, 2015=100
\item \code{crudeoil$risk} -- Geopolitical risk (GPR) index
\item \code{crudeoil$ex_rate} -- U.S. real effective exchange rate index (broad basket), 2020=100
}
}

\details{The data are in monthly frequency. They cover the period between Jan, 1998 and Oct, 2024.
}

\source{The data are provided by Bank for International Settlements, Board of Governors of the Federal Reserve System, Caldara and Iacoviello (2022), Federal Reserve Bank of Dallas, OECD, U.S. Energy Information Administration and World Bank. 

\url{https://www.bis.org}

\url{https://www.dallasfed.org} 

\url{https://www.eia.gov}

\url{https://www.federalreserve.gov}

\url{https://www.matteoiacoviello.com/gpr.htm}

\url{https://www.oecd.org/en.html} 

\url{https://www.worldbank.org/ext/en/home}
}

\examples{
data(crudeoil)
wti <- crudeoil[-1,1]
drivers <- (lag(crudeoil[,-1],k=1))[-1,]
ld.wti <- (diff(log(wti)))[-1,]
ld.drivers <- drivers[-1,]
ld.drivers[,c(4,6)] <- (diff(drivers[,c(4,6)]))[-1,]
ld.drivers[,c(1:2,5,7)] <- (diff(log(drivers[,c(1:2,5,7)])))[-1,]
ld.drivers[,c(3,6)] <- ld.drivers[,c(3,6)]/100
\donttest{
m <- fDMA(y=ld.wti,x=ld.drivers,alpha=0.99,lambda=0.99,initvar=1,model="dma")
}
}

\references{
Bank for International Settlements, 2025. Effective exchange rates, BIS WS_EER 1.0 (data set). \url{https://data.bis.org/topics/EER/BIS\%2CWS_EER\%2C1.0/M.R.B.US} 

Board of Governors of the Federal Reserve System, 2025. Selected interest rates. \url{https://www.federalreserve.gov/releases/h15/}

Caldara, D., Iacoviello, M., 2022. Measuring geopolitical risk. \emph{American Economic Review} \bold{112}, 1194--1225. 

Federal Reserve Bank of Dallas, 2025. Index of global real economic activity. \url{https://www.dallasfed.org/research/igrea}

Kilian, L., 2009. Not all oil price shocks are alike: Disentangling demand and supply shocks in the crude oil market. \emph{American Economic Review} \bold{99}, 1053--1069. 

OECD, 2025. Share prices. \url{https://www.oecd.org/en/data/indicators/share-prices.html}

U.S. Energy Information Administration, 2025. Petroleum /& other liquids. \url{https://www.eia.gov/petroleum/data.php}

World Bank, 2025. Commodity markets. \url{https://www.worldbank.org/en/research/commodity-markets}
}
