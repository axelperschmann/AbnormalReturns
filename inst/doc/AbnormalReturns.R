## ---- eval=FALSE---------------------------------------------------------
#  return.commodity = commodity[indices.Estimation, ][['Close']]
#  return.portfolio = portfolio[indices.Estimation, ][['Close']]
#  
#  M = lm(return.commodity ~ return.portfolio)

## ---- eval=FALSE---------------------------------------------------------
#  event.return.commodity = commodity[idx, ][['Close']]
#  event.return.portfolio = portfolio[idx, ][['Close']]
#  
#  nd = data.frame(return.portfolio = event.return.portfolio)
#  abnormal = event.return.commodity - predict(M, newdata = nd)

## ------------------------------------------------------------------------
d.DAX <- read.csv("../data-raw/DAX_2015.csv", stringsAsFactors=FALSE)
d.DAX$Date <- as.POSIXct(d.DAX$Date)
d.DAX <- d.DAX[order(d.DAX$Date),]
comment(d.DAX) <- 'DAX'
head(d.DAX)

d.Adidas <- read.csv("../data-raw/DAX_2015_Adidas.csv", stringsAsFactors=FALSE)
d.Adidas <- d.Adidas[d.Adidas$Volume != 0, ]
d.Adidas$Date <- as.POSIXct(d.Adidas$Date)
d.Adidas <- d.Adidas[order(d.Adidas$Date),]
comment(d.Adidas) <- 'Adidas'
head(d.Adidas)

## ------------------------------------------------------------------------
library(AbnormalReturns)
abnormal = computeAbnormalReturn(portfolio=d.DAX, commodity=d.Adidas)
head(abnormal)
summary(abnormal$R.squared)
summary(abnormal$abnormalReturn)

## ---- eval=FALSE---------------------------------------------------------
#  computeAbnormalReturn(portfolio=d.DAX, commodity=d.Adidas, showPlot=TRUE)
#  computeAbnormalReturn(portfolio=d.DAX, commodity=d.Adidas, showPlot=TRUE, eventIndex=125)

## ---- echo=FALSE---------------------------------------------------------
x = computeAbnormalReturn(portfolio=d.DAX, commodity=d.Adidas, showPlot=TRUE)
x = computeAbnormalReturn(portfolio=d.DAX, commodity=d.Adidas, showPlot=TRUE, eventIndex=125)

