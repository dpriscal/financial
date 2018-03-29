require(quantstrat)
require(PerformanceAnalytics)
require(lattice)

startDate <- '2010-01-01'
endDate <-  '2016-12-31'
Sys.setenv(TZ="Europe/Madrid") 
symbols <- c("ITX.MC", "SAN.MC", "TEF.MC", "BBVA.MC", "IBE.MC") 
getSymbols(symbols, from=startDate, to=endDate, index.class="POSIXct")

