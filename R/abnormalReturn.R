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
#' @param prices_market,prices_stock Data frames, minimally containing a column
#'   \code{Date} and a second column named as specified by
#'   \code{attributeOfInterest}. Time series data of prices_market and prices_stock
#'   performance respectively.
#' @param model A character object.
#' \itemize{
#'      \item \strong{\code{"marketmodel"}}
#'      \item \strong{\code{"constantmeanmodel"}}
#'    }
#'    All other types give an error.
#' @param eventIndex Positive integer, sequence of integers, or \code{NULL}.
#' If \code{eventIndex} is a positive integer or sequence of integers, function computes
#' abnormalReturns for the defined indices only. If \code{eventIndex=NULL}, function
#' computes abnormalReturns for all data points in the range of
#' \code{eventWindowLength+1:nrow{prices_market}}.
#' @param estimationWindowLength Positive integer. Defines number of
#'   observations considered to estimate model(s).
#' @param attributeOfInterest A character object. Defines the attribute of
#'   interest.
#' @param showPlot A boolean value. Should a plot of the prices_stock performance
#'   be shown?
#' @return \code{abnormalReturn} returns a data frame, comprising following columns:
#'   \itemize{
#'      \item \strong{\code{Date}} POSIXct.
#'      \item \strong{\code{abnormalReturn}} Numerical.
#'      \item \strong{\code{R.squared}} Numerical, between 0 and 1. The higher the
#'      \code{R.squared}, the greater is the variance reduction of the abnormal return
#'      - i.e. detecting the effect better.
#'      \item \strong{\code{stockReturn}} Numerical. Actual return of prices_stock.
#'      \item \strong{\code{marketReturn}} Numerical. Actual return of prices_market.
#'    }
#'    The number of rows returned depends on the length of \code{prices_stock}/\code{prices_market},
#'    as well as \code{estimationWindowLength} and \code{eventIndex}.
#' @examples
#' x <- abnormalReturn(prices_stock=d.VW, prices_market=d.DAX, model="marketmodel",
#'                     estimationWindowLength=10, attributeOfInterest="Close",
#'                     showPlot=TRUE)
#' head(x)
#' summary(x$R.squared)
#' summary(x$abnormalReturn)
#' @keywords abnormalReturn prices_market prices_stock stock performance financial
#'   markets values event impact
#' @importFrom stats lm predict
#' @importFrom utils data
#' @export
abnormalReturn <- function(prices_stock=NULL, prices_market=NULL,
                                  eventIndex = NULL,
                                  model = "marketmodel",
                                  estimationWindowLength = 10,
                                  attributeOfInterest = "Close",
                                  showPlot = FALSE) {
  if (is.null(prices_stock)) {
    stop("Argument 'prices_stock' is required.")
  }

  if (model == "marketmodel" && is.null(prices_market)) {
    stop("Argument 'prices_market' is required for the market model.")
  }

  ## validate parameters

  if (estimationWindowLength < 5) {
    stop("An estimation window size of at least 5 time steps is recommend for the regression to produce meaningful results.")
  }

  # check wether dataset contains attribute Date and attributeOfInterest
  if (!("Date" %in% names(prices_market))) {
    stop(paste0("Argument 'prices_market' data' does not contain attribute 'Date'. Available Attributes: ",
          paste(names(data), collapse = ", ")))
  }


  if (!("Date" %in% names(prices_stock))) {
    stop(paste0("Error! prices_stock data does not contain attribute 'Date'. Available Attributes: ",
                paste(names(data), collapse = ", ")))
  }

  if (!attributeOfInterest %in% names(prices_market)) {
    stop(paste0("Error! prices_market data does not contain attribute '",
                attributeOfInterest,
                "'. Available Attributes: ",
                paste(names(data), collapse = ", ")))
  }

  if (!attributeOfInterest %in% names(prices_stock)) {
    stop(paste0("Error! prices_stock data does not contain attribute '",
                attributeOfInterest,
                "'. Available Attributes: ",
                paste(names(data), collapse = ", "))
    )
  }

  # check wether prices_market and prices_stock data sets fit together
  if (nrow(prices_market) != nrow(prices_stock)) {
    stop(paste("Error! prices_market and prices_stock data should be of same length:",
               nrow(prices_market), "vs.", nrow(prices_stock)))
  }

  if (any(prices_market$Date != prices_stock$Date)) {
    stop("Error! Dates of prices_market and prices_stock data sets do not match.")
  }

  # check for which indices the abnormalReturns shall be calculated
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

    # data points of estimation window
    return.prices_stock = prices_stock[indices.Estimation,][[attributeOfInterest]]
    return.prices_market = prices_market[indices.Estimation,][[attributeOfInterest]]

    M = lm(return.prices_stock ~ return.prices_market)

    # compute abnormal return
    nd = data.frame(return.prices_market = prices_market[idx,][[attributeOfInterest]])
    abnormal = prices_stock[idx,][[attributeOfInterest]] - predict(M, newdata = nd)

    collect.abnRet = rbind(
      collect.abnRet,
      data.frame(
        Date = prices_market$Date[idx],
        abnormalReturn = abnormal,
        R.squared = summary(M)$r.squared,
        stockReturn = prices_stock[idx,][[attributeOfInterest]],
        marketReturn = prices_market[idx,][[attributeOfInterest]]
      )
    )
  }
  # name each row according to it"s corresponding index
  row.names(collect.abnRet) <- indices
  comment(collect.abnRet) <-
    paste("abnormalReturns for ",
          comment(prices_stock),
          " (",
          comment(prices_market),
          ")",
          sep = "")

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
  title <- paste0("prices_stock: ", comment(prices_stock),
                  " - prices_market: ", comment(prices_market),
                  "\n(", min(prices_stock$Date), " - ", max(prices_stock$Date), ")")
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
# d.DAX <- read.csv("data/DAX_2015.csv", stringsAsFactors=FALSE)
# d.DAX$Date <- as.POSIXct(d.DAX$Date)
# d.DAX <- d.DAX[order(d.DAX$Date),]
# comment(d.DAX) <- "DAX"
#
# d.Adidas <- read.csv("data/DAX_2015_Adidas.csv", stringsAsFactors=FALSE)
# d.Adidas <- d.Adidas[d.Adidas$Volume != 0, ]
# d.Adidas$Date <- as.POSIXct(d.Adidas$Date)
# d.Adidas <- d.Adidas[order(d.Adidas$Date),]
# comment(d.Adidas) <- "Adidas"
#
# d.VW <- read.csv("data/DAX_2015_VW.csv", stringsAsFactors=FALSE)
# d.VW <- d.VW[d.VW$Volume != 0, ]
# d.VW$Date <- as.POSIXct(d.VW$Date)
# d.VW <- d.VW[order(d.VW$Date),]
# comment(d.VW) <- "VW"
#
# # compute abnormal Returns
# abnormal = abnormalReturn(prices_market=d.DAX, prices_stock=d.VW, model="marketmodel",
#                           eventIndex=NULL, estimationWindowLength=20, attributeOfInterest="Close",
#                           showPlot=TRUE)
# print(mean(abnormal$R.squared))
# print(var(abnormal$R.squared))
