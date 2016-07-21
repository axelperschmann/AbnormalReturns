#' computeAbnormalReturns function
#'
#' This function computes the abnormal returns you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()
#' @importFrom utils read.csv
computeAbnormalReturn <- function(data=NULL, datasetname=NULL, orderBy='Date', showPlot=TRUE, attributeOfInterest='Close',
                                  eventWindowLength=1, eventWindowStart=NULL, estimationWindowLength=NULL,
                                  summary=FALSE) {
  # load dummy dataset if no data set is provided
  if (is.null(data)) {
    print("Loading dummy data set: SP500_2015.csv")
    data = read.csv("SP500_2015.csv", stringsAsFactors=FALSE)
    data$Date = as.POSIXct(data$Date)
    data <- data[order(data$Date),]
    datasetname="SP500"
  }

  # only allow positive values
  eventWindowLength = abs(eventWindowLength)
  if (!is.null(eventWindowStart)) {
    eventWindowStart = abs(eventWindowStart)
  }
  if (!is.null(estimationWindowLength)) {
    estimationWindowLength = abs(estimationWindowLength)
  }

  # Default value for eventWindowStart: last (eventWindowLength) data point(s) of dataset.
  if (is.null(eventWindowStart)) {
    eventWindowStart = nrow(data) - eventWindowLength + 1
  }

  # Default value for estimationWindowLength: all data points preceding the event window.
  if (is.null(estimationWindowLength)) {
    estimationWindowLength = eventWindowStart-1
  }

  # check for bad parameters
  if (eventWindowLength == 0) {
    stop("Error! eventWindowLength must not equal 0.")
  }
  if (eventWindowStart <= 2) {
    # a minimum of two points is necessary for a linear regression.
    stop("Error! Cannot estimate linear model, because no estimationWindowLength was provided.")
  }
  if (eventWindowStart + eventWindowLength -1 > nrow(data)) {
    stop("Error! To large indices for given dataset. Please check eventWindowStart, eventWindowLength and dataset")
  }
  if (eventWindowStart < estimationWindowLength) {
    estimationWindowLength = eventWindowStart - 1
    warning(paste("bad estimationWindowLength chosen. corrected to:", estimationWindowLength))
  }

  # check wether dataset contains queried attributes
  if (!orderBy %in% names(data)) {
    stop(paste("Error! Dataset does not contain attribute '", orderBy, "'. Available attributes: ", paste(names(data), collapse = ", "), sep=''))
  }
  if (!attributeOfInterest %in% names(data)) {
    stop(paste("Error! Dataset does not contain attribute '", attributeOfInterest, "'. Available Attributes: ", paste(names(data), collapse = ", "), sep=''))
  }

  # indices of (pre)event data points
  indices.Estimation = (eventWindowStart-estimationWindowLength):(eventWindowStart-1)
  indices.Event = eventWindowStart:(eventWindowStart + eventWindowLength - 1)

  # extract preEventWindow (=trainingset) and EventWindow (=testset) from given time series.
  data.train = data[indices.Estimation,]
  data.test = data[indices.Event,]

  # Linear regression via OLS
  response = data.train[[attributeOfInterest]]
  regressor = data.train[[orderBy]]

  M = lm(response ~ regressor)

  pred = predict(M, newdata=data.frame(regressor=data.test[[orderBy]]), interval="predict")
  abnormalReturns = pred[,1] - data.test[[attributeOfInterest]]

  if (showPlot==TRUE) {
    title = paste(datasetname, " (", min(data[[orderBy]]), " - ", max(data[[orderBy]]),")", sep="")
    subtitle = paste(round(abnormalReturns, digits=2), collapse = ", ")
    if (eventWindowLength==1) {
      subtitle = paste("abnormal return:", subtitle)
    } else {
      subtitle = paste("abnormal returns:", subtitle)
    }

    # whole time series
    plot(data[[orderBy]], data[[attributeOfInterest]], col='grey',
         ylab=paste("Daily", attributeOfInterest), xlab=orderBy, main=title, sub=subtitle,
         ylim=c(0.98*min(data[[attributeOfInterest]]), 1.02*max(data[[attributeOfInterest]])))
    legend('bottomleft', c("time series",
                           "pre-event window                                ",
                           paste("normal return (R^2 = ", round(summary(M)$r.squared, digits=2), ")", sep=''),
                           "actual return"),
           lty=FALSE, col=c('grey', 'red', 'green', 'blue'), cex=1, pch=4)

    # train/test dataset
    points(data.train[[orderBy]], data.train[[attributeOfInterest]], col='red', pch=4)
    points(data.test[[orderBy]],  data.test[[attributeOfInterest]],  col='blue', pch=4)
    abline(M, col="green", h = 0, lty = 2)

    # expected return
    points(data.test$Date, pred[,1], col="green", pch=4)

  }
  if (summary == TRUE) {
    print(summary(M))
  }
  return(abnormalReturns)
}

data = read.csv("SP500_2015.csv", stringsAsFactors=FALSE)
data$Date = as.POSIXct(data$Date)
data <- data[order(data$Date),]

abnormalReturns = computeAbnormalReturn(data=data, orderBy = 'Date', attributeOfInterest = 'Close',
                                        eventWindowLength = 1, eventWindowStart = 40, estimationWindowLength = 20,
                                        showPlot=TRUE, summary=FALSE)

