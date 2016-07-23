#' computeAbnormalReturns function
#'
#' This function computes the abnormal returns
#' @param love Do you love cats? Defaults to TRUE. (ToDo)
#' @keywords cats (ToDo)
#' @export
#' @examples
#' cat_function()
#' @importFrom utils read.csv
computeAbnormalReturn <- function(portfolio, commodity, regressionType='OLS',
                                  eventIndex=NULL, estimationWindowLength=20, attributeOfInterest = 'Close',
                                  showPlot=FALSE) {
  ## start by validating given parameters
  if (estimationWindowLength < 3) {
    # a minimum of two points is necessary for a linear regression.
    stop("Error! A minimum estimation window size of 3 is necessary for this linear regression to produce meaningful results.")
  }

  # check wether dataset contains attribute Date and attributeOfInterest
  if (!'Date' %in% names(portfolio)) {
    stop(paste("Error! Portfolio data does not contain attribute 'Date'. Available Attributes: ", paste(names(data), collapse = ", "), sep=''))
  }
  if (!'Date' %in% names(commodity)) {
    stop(paste("Error! Commodity data does not contain attribute 'Date'. Available Attributes: ", paste(names(data), collapse = ", "), sep=''))
  }
  if (!attributeOfInterest %in% names(portfolio)) {
    stop(paste("Error! Portfolio data does not contain attribute '", attributeOfInterest, "'. Available Attributes: ", paste(names(data), collapse = ", "), sep=''))
  }
  if (!attributeOfInterest %in% names(commodity)) {
    stop(paste("Error! Commodity data does not contain attribute '", attributeOfInterest, "'. Available Attributes: ", paste(names(data), collapse = ", "), sep=''))
  }

  # check wether portfolio and commodity data sets fit together
  if (nrow(portfolio) != nrow(commodity)) {
    stop(paste("Error! Portfolio and commodity data should be of same length:", nrow(portfolio), "vs.", nrow(commodity)))
  }
  for (check in (portfolio$Date == commodity$Date)) {
    if (check == FALSE) {
      stop("Error! Dates of portfolio and commodity data sets do not match")
    }
  }

  # check for which indices the abnormalReturns shall be calculated
  if (is.null(eventIndex)) {
    indices = (estimationWindowLength+1):nrow(portfolio)
  } else {
    if (eventIndex > (estimationWindowLength)) {
      indices = eventIndex
    }
    else {
      stop("Error! Chosen eventIndex overlaps with estimationWindow. Choose higher eventIndex!")
    }
  }

  collect.abnRet = c()
  for (idx in indices) {
    # indices of data points used for estimating the OLS model
    indices.Estimation = (idx-estimationWindowLength):(idx-1)

    # data points of estimation window
    return.commodity = commodity[indices.Estimation,][[attributeOfInterest]]
    return.portfolio = portfolio[indices.Estimation,][[attributeOfInterest]]

    switch(regressionType,
           OLS={
             # Ordinary Least Squares
             M = lm(return.commodity ~ return.portfolio)
           },
           {
             # default case
             stop(paste("Error! Unknown regressionType specified:", regressionType))
           })


    # event data points
    event.return.commodity = commodity[idx,][[attributeOfInterest]]
    event.return.portfolio = portfolio[idx,][[attributeOfInterest]]

    # compute abnormal return
    nd = data.frame(return.portfolio = event.return.portfolio)
    abnormal= event.return.commodity - predict(M, newdata=nd)

    collect.abnRet = rbind(collect.abnRet,
                           data.frame(Date=portfolio$Date[idx],
                                      abnormalReturn=abnormal,
                                      R.squared=summary(M)$r.squared,
                                      commodityReturn=event.return.commodity,
                                      portfolioReturn=event.return.portfolio)
                                      )
  }
  # name each row according to it's corresponding index
  row.names(collect.abnRet) <- indices
  comment(collect.abnRet) <- paste("abnormalReturns for ", comment(commodity), " (", comment(portfolio), ")", sep="")

  if (showPlot==TRUE) {
    title = paste("Commodity: ", comment(commodity), " - Portfolio: ", comment(portfolio), "\n(", min(commodity$Date), " - ", max(commodity$Date),")", sep="")
    subtitle = paste('Length of estimation window:', estimationWindowLength)

    # plot whole time series of given commodity
    plot(commodity$Date, commodity[[attributeOfInterest]], col='grey',
         ylab=paste("Daily", attributeOfInterest), xlab='Date', main=title, sub=subtitle)

    legend('topleft', c("abnormalReturn negative                ", "abnormalReturn positive"),
           lty=FALSE, col=c('red', 'green'), cex=1, pch=4)

    # mark events with positive and negative abnormal returns accordingly.
    neg = collect.abnRet[collect.abnRet$abnormalReturn <= 0,]
    pos = collect.abnRet[collect.abnRet$abnormalReturn > 0,]
    points(neg$Date, neg$commodityReturn, col='red', pch=4)
    points(pos$Date, pos$commodityReturn, col='green', pch=4)
  }

  return(collect.abnRet)
}

# load data
d.DAX = read.csv("DAX_2015.csv", stringsAsFactors=FALSE)
d.DAX$Date = as.POSIXct(d.DAX$Date)
d.DAX <- d.DAX[order(d.DAX$Date),]
comment(d.DAX) = 'DAX'

d.Adidas = read.csv("DAX_2015_Adidas.csv", stringsAsFactors=FALSE)
d.Adidas <- d.Adidas[d.Adidas$Volume != 0, ]
d.Adidas$Date = as.POSIXct(d.Adidas$Date)
d.Adidas <- d.Adidas[order(d.Adidas$Date),]
comment(d.Adidas) = 'Adidas'

d.VW = read.csv("DAX_2015_VW.csv", stringsAsFactors=FALSE)
d.VW <- d.VW[d.VW$Volume != 0, ]
d.VW$Date = as.POSIXct(d.VW$Date)
d.VW <- d.VW[order(d.VW$Date),]
comment(d.VW) = 'VW'

# compute abnormal Returns
abnormal = computeAbnormalReturn(portfolio=d.DAX, commodity=d.VW, regressionType='OLS',
                                 eventIndex=NULL, estimationWindowLength=20, attributeOfInterest='Close',
                                 showPlot=TRUE)
print(mean(abnormal$R.squared))
print(var(abnormal$R.squared))
