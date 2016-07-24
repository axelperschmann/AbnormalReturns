#' computeAbnormalReturns function.
#'
#' @description \code{computeAbnormalReturn} implements the event study
#'   methodology and abnormal returns in particular. The event study methodology
#'   is a common way to study the effects of certain events on stock prices. It
#'   thus calculates a so-called abnormal return that measures the impact
#'   without confounding influences. As part of this method, one first has to
#'   predict a normal return in the absence of the event under study.
#'   Afterwards, one calculates the difference between the actual return and the
#'   previously predicted normal return, i.e. the abnormal return.
#'
#' @param portfolio,commodity Data frames, minimally containing a column
#'   \code{Date} and a second column named as specified by
#'   \code{attributeOfInterest}. Time series data of portfolio and commodity
#'   performance respectively.
#' @param regressionType A character object. Possible types are:
#'   \itemize{
#'      \item \strong{\code{'OLS'}} for Ordinary Least Squares.
#'    }
#'    All other types give an error.
#' @param eventIndex Positive integer, sequence of integers, or \code{NULL}.
#' If \code{eventIndex} is a positive integer or sequence of integers, function computes
#' abnormalReturns for the defined indices only. If \code{eventIndex=NULL}, function
#' computes abnormalReturns for all data points in the range of
#' \code{eventWindowLength+1:nrow{portfolio}}.
#' @param estimationWindowLength Positive integer. Defines number of
#'   observations considered to estimate model(s).
#' @param attributeOfInterest A character object. Defines the attribute of
#'   interest.
#' @param showPlot A boolean value. Should a plot of the commodity performance
#'   be shown?
#' @return \code{computeAbnormalReturn} returns a data frame, comprising following columns:
#'   \itemize{
#'      \item \strong{\code{Date}} POSIXct.
#'      \item \strong{\code{abnormalReturn}} Numerical.
#'      \item \strong{\code{R.squared}} Numerical, between 0 and 1. The higher the
#'      \code{R.squared}, the greater is the variance reduction of the abnormal return
#'      - i.e. detecting the effect better.
#'      \item \strong{\code{commodityReturn}} Numerical. Actual return of commodity.
#'      \item \strong{\code{portfolioReturn}} Numerical. Actual return of portfolio.
#'    }
#'    The number of rows returned depends on the length of \code{commodity}/\code{portfolio},
#'    as well as \code{estimationWindowLength} and \code{eventIndex}.
#' @keywords abnormalReturn portfolio commodity stock performance financial
#'   markets values event impact
#' @export
#' @examples
#' x <- computeAbnormalReturn(portfolio=d.DAX, commodity=d.VW, regressionType='OLS',
#'                            estimationWindowLength=20, attributeOfInterest='Close',
#'                            showPlot=TRUE)
#' head(x)
#' summary(x$R.squared)
#' summary(x$abnormalReturn)
#' @importFrom utils read.csv
#' @importFrom graphics plot legend points
#' @importFrom stats lm predict
#' @importFrom utils data
computeAbnormalReturn <-
  function(portfolio,
           commodity,
           regressionType = 'OLS',
           eventIndex = NULL,
           estimationWindowLength = 20,
           attributeOfInterest = 'Close',
           showPlot = FALSE) {
    ## start by validating given parameters
    if (estimationWindowLength < 3) {
      # a minimum of two points is necessary for a linear regression.
      stop(
        "Error! A minimum estimation window size of 3 is necessary for this linear regression to produce meaningful results."
      )
    }

    # check wether dataset contains attribute Date and attributeOfInterest
    if (!'Date' %in% names(portfolio)) {
      stop(
        paste(
          "Error! Portfolio data does not contain attribute 'Date'. Available Attributes: ",
          paste(names(data), collapse = ", "),
          sep = ''
        )
      )
    }
    if (!'Date' %in% names(commodity)) {
      stop(
        paste(
          "Error! Commodity data does not contain attribute 'Date'. Available Attributes: ",
          paste(names(data), collapse = ", "),
          sep = ''
        )
      )
    }
    if (!attributeOfInterest %in% names(portfolio)) {
      stop(
        paste(
          "Error! Portfolio data does not contain attribute '",
          attributeOfInterest,
          "'. Available Attributes: ",
          paste(names(data), collapse = ", "),
          sep = ''
        )
      )
    }
    if (!attributeOfInterest %in% names(commodity)) {
      stop(
        paste(
          "Error! Commodity data does not contain attribute '",
          attributeOfInterest,
          "'. Available Attributes: ",
          paste(names(data), collapse = ", "),
          sep = ''
        )
      )
    }

    # check wether portfolio and commodity data sets fit together
    if (nrow(portfolio) != nrow(commodity)) {
      stop(
        paste(
          "Error! Portfolio and commodity data should be of same length:",
          nrow(portfolio),
          "vs.",
          nrow(commodity)
        )
      )
    }
    for (check in (portfolio$Date == commodity$Date)) {
      if (check == FALSE) {
        stop("Error! Dates of portfolio and commodity data sets do not match.")
      }
    }

    # check for which indices the abnormalReturns shall be calculated
    if (is.null(eventIndex)) {
      indices = (estimationWindowLength + 1):nrow(portfolio)
    } else {
      if (eventIndex > (estimationWindowLength)) {
        indices = eventIndex
      }
      else {
        stop(
          "Error! Chosen eventIndex overlaps with estimationWindow. Choose higher eventIndex!"
        )
      }
    }

    collect.abnRet = c()
    for (idx in indices) {
      # indices of data points used for estimating the OLS model
      indices.Estimation = (idx - estimationWindowLength):(idx - 1)

      # data points of estimation window
      return.commodity = commodity[indices.Estimation, ][[attributeOfInterest]]
      return.portfolio = portfolio[indices.Estimation, ][[attributeOfInterest]]

      switch(regressionType,
             OLS = {
               # Ordinary Least Squares
               M = lm(return.commodity ~ return.portfolio)
             },
             {
               # default case
               stop(paste(
                 "Error! Unknown regressionType specified:",
                 regressionType
               ))
             })


      # event data points
      event.return.commodity = commodity[idx, ][[attributeOfInterest]]
      event.return.portfolio = portfolio[idx, ][[attributeOfInterest]]

      # compute abnormal return
      nd = data.frame(return.portfolio = event.return.portfolio)
      abnormal = event.return.commodity - predict(M, newdata = nd)

      collect.abnRet = rbind(
        collect.abnRet,
        data.frame(
          Date = portfolio$Date[idx],
          abnormalReturn = abnormal,
          R.squared = summary(M)$r.squared,
          commodityReturn = event.return.commodity,
          portfolioReturn = event.return.portfolio
        )
      )
    }
    # name each row according to it's corresponding index
    row.names(collect.abnRet) <- indices
    comment(collect.abnRet) <-
      paste("abnormalReturns for ",
            comment(commodity),
            " (",
            comment(portfolio),
            ")",
            sep = "")

    if (showPlot == TRUE) {
      title = paste(
        "Commodity: ",
        comment(commodity),
        " - Portfolio: ",
        comment(portfolio),
        "\n(",
        min(commodity$Date),
        " - ",
        max(commodity$Date),
        ")",
        sep = ""
      )
      subtitle = paste('Length of estimation window:', estimationWindowLength)

      # plot whole time series of given commodity
      plot(
        commodity$Date,
        commodity[[attributeOfInterest]],
        col = 'grey',
        ylab = paste("Daily", attributeOfInterest),
        xlab = 'Date',
        main = title,
        sub = subtitle
      )

      legend(
        'topleft',
        c("abnormalReturn negative                ", "abnormalReturn positive"),
        lty = FALSE,
        col = c('red', 'green'),
        cex = 1,
        pch = 4
      )

      # mark events with positive and negative abnormal returns accordingly.
      neg = collect.abnRet[collect.abnRet$abnormalReturn <= 0, ]
      pos = collect.abnRet[collect.abnRet$abnormalReturn > 0, ]
      points(neg$Date,
             neg$commodityReturn,
             col = 'red',
             pch = 4)
      points(pos$Date,
             pos$commodityReturn,
             col = 'green',
             pch = 4)
    }

    return(collect.abnRet)
  }

# # load data
# d.DAX <- read.csv("data/DAX_2015.csv", stringsAsFactors=FALSE)
# d.DAX$Date <- as.POSIXct(d.DAX$Date)
# d.DAX <- d.DAX[order(d.DAX$Date),]
# comment(d.DAX) <- 'DAX'
#
# d.Adidas <- read.csv("data/DAX_2015_Adidas.csv", stringsAsFactors=FALSE)
# d.Adidas <- d.Adidas[d.Adidas$Volume != 0, ]
# d.Adidas$Date <- as.POSIXct(d.Adidas$Date)
# d.Adidas <- d.Adidas[order(d.Adidas$Date),]
# comment(d.Adidas) <- 'Adidas'
#
# d.VW <- read.csv("data/DAX_2015_VW.csv", stringsAsFactors=FALSE)
# d.VW <- d.VW[d.VW$Volume != 0, ]
# d.VW$Date <- as.POSIXct(d.VW$Date)
# d.VW <- d.VW[order(d.VW$Date),]
# comment(d.VW) <- 'VW'
#
# # compute abnormal Returns
# abnormal = computeAbnormalReturn(portfolio=d.DAX, commodity=d.VW, regressionType='OLS',
#                                  eventIndex=NULL, estimationWindowLength=20, attributeOfInterest='Close',
#                                  showPlot=TRUE)
# print(mean(abnormal$R.squared))
# print(var(abnormal$R.squared))
