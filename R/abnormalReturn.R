#' Calculation of abnormal returns
#'
#' This functions implements the event study
#'   methodology and abnormal returns in particular. The event study methodology
#'   is a common way to study the effects of certain events on stock prices. It
#'   thus calculates a so-called abnormal return that measures the impact
#'   without confounding influences. As part of this method, one first has to
#'   predict a normal return in the absence of the event under study.
#'   Afterwards, one calculates the difference between the actual return and the
#'   previously predicted normal return, i.e. the abnormal return.
#'
#' @param prices_stock,prices_market Data frames, minimally containing a column
#'   \code{Date} and a second column named as specified by
#'   \code{attributeOfInterest}. Time series data of prices_market and prices_stock
#'   performance respectively. \strong{OR:} Character objects, representing stock symbols.
#'   These symbols are used to query data directly from the Yahoo Finance platform.
#' @param from,to Character objects. Defining the data range.
#' @param model A character object.
#' \itemize{
#'      \item \strong{\code{"marketmodel"}}
#'      \item \strong{\code{"constantmeanmodel"}}
#'      Argument 'prices_market' is not required for the constant mean model
#'    }
#'    All other types give an error.
#' @param eventIndex Positive integer, sequence of integers, or \code{NULL}.
#' If \code{eventIndex} is a positive integer or sequence of integers, function computes
#' abnormalReturns for the defined indices only. If \code{eventIndex=NULL}, function
#' computes abnormalReturns for all data points in the range of
#' \code{eventWindowLength+1:nrow{prices_market}}.
#' @param estimationWindowLength Positive integer. Defines number of
#'   observations considered to estimate model(s).
#' @param c Positive integer. Cumulative Abnormal Returns, defines the number of abnormal returns to cumulate.
#' @param attributeOfInterest A character object. Defines the attribute of
#'   interest.
#' @param showPlot A boolean value. Should a plot of the prices_stock performance
#'   be shown?
#' @return \code{abnormalReturn} returns a data frame, comprising following columns:
#'   \itemize{
#'      \item \strong{\code{Date}} POSIXct.
#'      \item \strong{\code{abnormalReturn}} Numerical.
#'      \item \strong{\code{cumulativeAbnormalReturn}} Numerical. The first \code{c-1} rows will be \code{cumulativeAbnormalReturn=NA}.
#'      \item \strong{\code{stockReturn}} Numerical. Actual return of prices_stock.
#'    }
#'    The number of rows returned depends on the length of \code{prices_stock}/\code{prices_market},
#'    as well as \code{estimationWindowLength} and \code{eventIndex}.
#' @examples
#' x <- abnormalReturn(prices_stock=d.VW, prices_market=d.DAX, model="marketmodel",
#'                     estimationWindowLength=10, c=10,
#'                     attributeOfInterest="Close", showPlot=TRUE)
#' head(x)
#' summary(x$abnormalReturn)
#' @keywords abnormalReturn prices_market prices_stock stock performance financial
#'   markets values event impact
#' @importFrom stats lm predict
#' @importFrom utils data
#' @importFrom quantmod getSymbols
#' @importFrom zoo index coredata
#' @import quantmod
#' @export
abnormalReturn <- function(prices_stock, prices_market=NULL, from="2015-01-01", to="2015-12-31",
                           eventIndex = NULL,
                           model = "marketmodel",
                           estimationWindowLength = 10, c = 10, attributeOfInterest = "Close",
                           showPlot = FALSE) {

  if (typeof(prices_stock) == 'character') {
    # no data given, utilize 'quantmod' to download data from Yahoo Finance.
    prices_stock.symbol = prices_stock

    data <- getSymbols(prices_stock.symbol, src='yahoo', from=from, to=to, auto.assign=FALSE)

    prices_stock <- data.frame(Date=index(data), coredata(data))
    names(prices_stock) = c('Date', 'Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted')
    comment(prices_stock) = prices_stock.symbol
    prices_stock$Date <- as.POSIXct(prices_stock$Date)
    prices_stock <- prices_stock[order(prices_stock$Date),]

    if (typeof(prices_market) == 'character') {
      prices_market.symbol = prices_market
      data <- getSymbols(prices_market.symbol, src='yahoo', from=from, to=to, auto.assign=FALSE)

      prices_market <- data.frame(Date=index(data), coredata(data))
      names(prices_market) <- c('Date', 'Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted')
      comment(prices_market) = prices_market.symbol
      prices_market$Date <- as.POSIXct(prices_market$Date)
      prices_market <- prices_market[order(prices_market$Date),]

      # make sure both data sets contain the same dates.
      prices_stock = prices_stock[prices_stock$Date %in% prices_market$Date,]
      prices_market = prices_market[prices_market$Date %in% prices_stock$Date,]
    }
  }

  # validate arguments
  validate_prices(prices_stock, prices_market, attributeOfInterest)

  if (model == "marketmodel" && is.null(prices_market)) {
    stop("Argument 'prices_market' is required for the market model.")
  }

  if (estimationWindowLength < 5) {
    stop("An estimation window size of at least 5 time steps is recommend for the regression to produce meaningful results.")
  }

  # select indices for which the abnormalReturns shall be calculated
  if (is.null(eventIndex)) {
    indices <- (estimationWindowLength + 1):nrow(prices_market)
  } else if (eventIndex > (estimationWindowLength)) {
    indices <- eventIndex
  } else {
    stop("Error! Chosen 'eventIndex' overlaps with 'estimationWindow'. Choose higher eventIndex.")
  }

  collect.abnRet = c()
  for (idx in indices) {
    # indices of data points used for estimating the OLS model
    indices.Estimation = (idx - estimationWindowLength):(idx - 1)

    # compute abnormal return
    switch(model,
           'marketmodel' = {
             normalReturn = compute_normalReturn.marketmodel(prices_stock[indices.Estimation,][[attributeOfInterest]],
                                                             prices_market[indices.Estimation,][[attributeOfInterest]],
                                                             prices_market[idx,][[attributeOfInterest]])
           },
           'constantmeanmodel' = {
             normalReturn = compute_normalReturn.constantmeanmodel(prices_stock[indices.Estimation,][[attributeOfInterest]])
           },
           {
             # default case
             stop(paste("Error! Unknown normal return model specified:",
                        model))
           })

    abnormal = prices_stock[idx,][[attributeOfInterest]] - normalReturn

    row = data.frame(
      Date = prices_market$Date[idx],
      abnormalReturn = abnormal,
      cumulativeAbnormalReturn = NA,
      stockReturn = prices_stock[idx,][[attributeOfInterest]]
    )

    collect.abnRet = rbind(collect.abnRet, row)
  }
  # name each row according to it"s corresponding index
  row.names(collect.abnRet) <- indices

  if (c >= 2 && nrow(collect.abnRet)>c) {
    for (i in (c):nrow(collect.abnRet)) {
      collect.abnRet$cumulativeAbnormalReturn[i] <- sum(collect.abnRet$abnormalReturn[(i-c):i])
    }
  } else if (c < 2) {
    warning("Warning! Accumulating abnormalReturns makes only sense for c >= 2.")
  }

  if (showPlot == TRUE) {
    plotEventStudy(prices_stock,
                   prices_market,
                   attributeOfInterest,
                   estimationWindowLength,
                   collect.abnRet)
  }

  return(collect.abnRet)
}


validate_prices <- function(prices_stock, prices_market, attributeOfInterest) {
  if (is.null(prices_stock)) {
    stop("Argument 'prices_stock' is required.")
  }

  if (!("Date" %in% names(prices_stock)) || !attributeOfInterest %in% names(prices_stock) ) {
    stop(paste0("Error! Please check prices_stock attributes'. Available Attributes: ",
                paste(names(prices_stock), collapse = ", ")))
  }

  if (!is.null(prices_market)) {
    if (!("Date" %in% names(prices_market)) || !attributeOfInterest %in% names(prices_market) ) {
      stop(paste0("Error! Please check prices_market attributes'. Available Attributes: ",
                  paste(names(prices_market), collapse = ", ")))
    }

    # check wether prices_market and prices_stock data sets fit together
    if (nrow(prices_market) != nrow(prices_stock)) {
      stop(paste("Error! prices_market and prices_stock data should be of same length:",
                 nrow(prices_market), "vs.", nrow(prices_stock)))
    }

    if (any(prices_market$Date != prices_stock$Date)) {
      stop("Error! Dates of prices_market and prices_stock data sets do not match.")
    }
  }
}


#' @importFrom graphics plot legend points
plotEventStudy <- function(prices_stock, prices_market,
                           attributeOfInterest,
                           estimationWindowLength,
                           collect.abnRet) {
  if (!is.null(prices_market)) {
    title <- paste0("stock: '", comment(prices_stock),
                    "' - market: '", comment(prices_market),
                    "'\n(", min(prices_stock$Date), " - ", max(prices_stock$Date), ")")
  } else {
    title <- paste0("stock: '", comment(prices_stock),
                    "'\n(", min(prices_stock$Date), " - ", max(prices_stock$Date), ")")
  }

  subtitle <- paste("Length of estimation window:", estimationWindowLength)

  # plot whole time series of given prices_stock
  plot(prices_stock$Date, prices_stock[[attributeOfInterest]],
       col = "grey",
       xlab = "Date", ylab = paste("Daily", attributeOfInterest),
       main = title,
       sub = subtitle)

  legend("topleft", c("abnormalReturn negative                ",
                      "abnormalReturn positive"),
         lty = FALSE,
         col = c("red", "green"),
         cex = 1, pch = 4)

  # mark events with positive and negative abnormal returns accordingly.
  neg = collect.abnRet[collect.abnRet$abnormalReturn <= 0, ]
  pos = collect.abnRet[collect.abnRet$abnormalReturn > 0, ]

  points(neg$Date,
         neg$stockReturn,
         col = "red",
         pch = 4)
  points(pos$Date,
         pos$stockReturn,
         col = "green",
         pch = 4)
}

# # load data
# d.DAX <- read.csv("data-raw/DAX_2015.csv", stringsAsFactors=FALSE)
# d.DAX$Date <- as.POSIXct(d.DAX$Date)
# d.DAX <- d.DAX[order(d.DAX$Date),]
# comment(d.DAX) <- "DAX"
#
# d.Adidas <- read.csv("data-raw/DAX_2015_Adidas.csv", stringsAsFactors=FALSE)
# d.Adidas <- d.Adidas[d.Adidas$Volume != 0, ]
# d.Adidas$Date <- as.POSIXct(d.Adidas$Date)
# d.Adidas <- d.Adidas[order(d.Adidas$Date),]
# comment(d.Adidas) <- "Adidas"
#
# d.VW <- read.csv("data-raw/DAX_2015_VW.csv", stringsAsFactors=FALSE)
# d.VW <- d.VW[d.VW$Volume != 0, ]
# d.VW$Date <- as.POSIXct(d.VW$Date)
# d.VW <- d.VW[order(d.VW$Date),]
# comment(d.VW) <- "VW"
#
# # compute abnormal Returns
# abnormal = abnormalReturn(prices_market=d.DAX, prices_stock=d.VW, model="marketmodel",
#                           eventIndex=NULL, estimationWindowLength=20, c=3, attributeOfInterest="Close",
#                           showPlot=TRUE)
# abnormal = abnormalReturn(prices_market="%5EGDAXI", prices_stock="VW.SW", model="marketmodel",
#                           eventIndex=NULL, estimationWindowLength=20, c=3, attributeOfInterest="Close",
#                           showPlot=TRUE)
