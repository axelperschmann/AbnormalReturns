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
#' @param from,to Character or Date objects. Defining the data range.
#' @param model A character object.
#' \itemize{
#'      \item \strong{\code{"marketmodel"}}
#'      \item \strong{\code{"constantmeanmodel"}}
#'      Argument 'prices_market' is not required for the constant mean model
#'    }
#'    All other types give an error.
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
#'      \item \strong{\code{cumulativeAbnormalReturn}} Numerical. The first \code{c-1} rows will be
#'            \code{cumulativeAbnormalReturn=NA}.
#'      \item \strong{\code{stockReturn}} Numerical. Actual return of prices_stock.
#'    }
#'    The number of rows returned depends on the length of \code{prices_stock}/\code{prices_market},
#'    as well as \code{estimationWindowLength} and \code{eventIndex}.
#' @examples
#' x <- abnormalReturn(prices_stock=d.VW, prices_market=d.DAX, model="marketmodel",
#'                     estimationWindowLength=10, c=10, attributeOfInterest="Close", showPlot=TRUE)
#' head(x)
#' summary(x$abnormalReturn)
#' x <- abnormalReturn(prices_stock="VOW3.DE", prices_market="%5EGDAXI", from="2015-03-01",
#'                     to="2015-11-30", model="marketmodel", estimationWindowLength=20, c=3,
#'                     attributeOfInterest="Close", showPlot=TRUE)
#' head(x)
#' summary(x$abnormalReturn)
#' @keywords abnormalReturn prices_market prices_stock stock performance financial
#'   markets values event impact
#' @importFrom stats lm predict
#' @importFrom utils data
#' @importFrom quantmod getSymbols
#' @importFrom zoo index coredata
#' @export
abnormalReturn <- function(prices_stock, prices_market=NULL, from=NULL, to=NULL,
                           model = "marketmodel",
                           estimationWindowLength = 10, c = 10, attributeOfInterest = "Close",
                           showPlot = FALSE) {

  if (is.null(prices_stock)) {
    stop("Argument 'prices_stock' is required.")
  }

  if (class(model) != 'character') {
    stop("Argument 'model' must be a character object.")
  }

  if (class(attributeOfInterest) != 'character') {
    stop("Argument 'attributeOfInterest' must be a character object.")
  }

  if (class(estimationWindowLength) != 'numeric') {
    stop("Argument 'estimationWindowLength' must be a numeric object")
  }

  if (class(c) != 'numeric') {
    stop("Argument 'c' must be a numeric object")
  }

  if (class(showPlot) != 'logical') {
    stop("Argument 'showPlot' must be a logical object")
  }

  if (model == "marketmodel" && is.null(prices_market)) {
    stop("Argument 'prices_market' is required for the market model.")
  }

  if (class(prices_stock) == 'character') {
    # if prices_stock contains a stock symbol, utilize 'quantmod' to download data from Yahoo Finance.
    prices_stock.symbol = prices_stock

    if (is.null(from) || is.null(to)) {
      stop("Argument 'from' and 'to' are required.")
    }

    if (as.POSIXct(from) > as.POSIXct(to)) {
      stop("Argument 'from' must define a date preceding that one specified in argument 'to'.")
    }

    options("getSymbols.warning4.0"=FALSE)
    data <- getSymbols(prices_stock.symbol, src='yahoo', from=from, to=to, auto.assign=FALSE)

    prices_stock <- data.frame(Date=index(data), coredata(data))
    names(prices_stock) = c('Date', 'Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted')
    comment(prices_stock) = prices_stock.symbol
    prices_stock$Date <- as.POSIXct(prices_stock$Date)
    prices_stock <- prices_stock[order(prices_stock$Date),]

    if (class(prices_market) == 'character') {
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

  if(class(prices_stock) != 'data.frame') {
    stop("Argument 'prices_stock' must be a data.frame")
  }

  if(class(prices_market) != 'NULL' && class(prices_market) != 'data.frame') {
    stop("Argument 'prices_market' must be a data.frame or NULL")
  }

  # create subset of data.frame, according to arguments 'from' and 'to'.
  if (!is.null(from)) {
    if (!is.null(to) && as.POSIXct(from) >= as.POSIXct(to)) {
        stop("Error! 'from' must be a date preceding 'to'.")
      }
    prices_stock = prices_stock[prices_stock$Date > as.POSIXct(from),]
    if (!is.null(prices_market)) {
      prices_market = prices_market[prices_market$Date > as.POSIXct(from),]
    }
  }
  if (!is.null(to)) {
    prices_stock = prices_stock[prices_stock$Date < as.POSIXct(to),]
    if (!is.null(prices_market)) {
      prices_market = prices_market[prices_market$Date < as.POSIXct(to),]
    }
  }

  if (nrow(prices_stock) < (estimationWindowLength + 1)) {
    stop(paste("Error! Not enough data points in 'prices_stock'. Number of rows:", nrow(prices_stock)))
  }

  # check if queried data.frame attributes exist.
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

  if (estimationWindowLength < 5) {
    stop("An estimation window size of at least 5 time steps is recommend for the regression to produce meaningful results.")
  }

  indices = (estimationWindowLength + 1):nrow(prices_stock)
  collect.abnRet = c()
  for (idx in indices) {
    # indices of data points used for estimating the OLS model
    indices.Estimation = (idx - estimationWindowLength):(idx - 1)

    # compute abnormal return
    if (model == 'marketmodel') {
      normalReturn = compute_normalReturn.marketmodel(prices_stock[indices.Estimation,][[attributeOfInterest]],
                                                      prices_market[indices.Estimation,][[attributeOfInterest]],
                                                      prices_market[idx,][[attributeOfInterest]])
    } else if (model == 'constantmeanmodel') {
      normalReturn = compute_normalReturn.constantmeanmodel(prices_stock[indices.Estimation,][[attributeOfInterest]])
    } else {
      # default case
      stop(paste("Error! Unknown normal return model specified:", model))
    }

    abnormal = prices_stock[idx,][[attributeOfInterest]] - normalReturn

    row = data.frame(
      Date = prices_stock$Date[idx],
      abnormalReturn = abnormal,
      cumulativeAbnormalReturn = NA,
      stockReturn = prices_stock[idx,][[attributeOfInterest]]
    )

    collect.abnRet = rbind(collect.abnRet, row)
  }
  # name each row according to its corresponding index.
  row.names(collect.abnRet) <- indices

  # Add CumulativeAdbnormalReturns
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


#' @importFrom graphics plot legend points
plotEventStudy <- function(prices_stock, prices_market,
                           attributeOfInterest,
                           estimationWindowLength,
                           collect.abnRet) {
  if (!is.null(prices_market)) {
    title <- paste0("stock: '", comment(prices_stock),
                    "' - market: '", comment(prices_market),
                    "'\n(", format(min(prices_stock$Date), format="%Y-%m-%d"), " - ",
                    format(max(prices_stock$Date), format="%Y-%m-%d"), ")")
  } else {
    title <- paste0("stock: '", comment(prices_stock),
                    "'\n(", format(min(prices_stock$Date), format="%Y-%m-%d"), " - ",
                    format(max(prices_stock$Date), format="%Y-%m-%d"), ")")
  }

  subtitle <- paste("Length of estimation window:", estimationWindowLength)

  # plot whole time series of given prices_stock
  # p <- qplot(prices_stock$Date, prices_stock[[attributeOfInterest]], main=title, xlab='Date', ylab=paste("Daily", attributeOfInterest))
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
# compute abnormal Returns
# abnormal = abnormalReturn(prices_stock=d.VW, prices_market=d.DAX, model="marketmodel", to = "2015-05-01",
#                           estimationWindowLength=20, c=3, attributeOfInterest="Close", showPlot=TRUE)
# abnormal = abnormalReturn(prices_stock="VOW3.DE", prices_market="%5EGDAXI", model="marketmodel",
#                           estimationWindowLength=20, c=3, attributeOfInterest="Close", showPlot=TRUE)
#
# abnormal = abnormalReturn(prices_stock="ADS.DE", prices_market="%5EGDAXI", model="marketmodel", from="2015-01-01", to="2015-12-31",
#                           estimationWindowLength=20, c=3, attributeOfInterest="Close", showPlot=TRUE)
