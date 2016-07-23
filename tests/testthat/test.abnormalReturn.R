library(testthat)
context("Abnormal returns")

test_that("returned data frame has correct dimensions (eventIndex==NULL)", {
  windowLength = 20
  x <- computeAbnormalReturn(portfolio=d.DAX, commodity=d.VW, eventIndex=NULL,
                             estimationWindowLength=windowLength)
  expect_equal(nrow(x), nrow(d.DAX) - windowLength)
  expect_equal(names(x), c("Date", "abnormalReturn", "R.squared", "commodityReturn", "portfolioReturn"))
})

test_that("returned data frame has correct dimensions (eventIndex!=NULL)", {
  x <- computeAbnormalReturn(portfolio=d.DAX, commodity=d.VW, eventIndex=50)
  expect_equal(nrow(x), 1)
  expect_equal(names(x), c("Date", "abnormalReturn", "R.squared", "commodityReturn", "portfolioReturn"))
})

test_that("portfolio and commodity are checked for conformity", {
  expect_that(
    computeAbnormalReturn(portfolio=d.DAX[1:252,], commodity=d.VW),
    throws_error("Error! Portfolio and commodity data should be of same length: 252 vs. 253")
  )

  expect_that(
    computeAbnormalReturn(portfolio=d.DAX, commodity=d.VW[1:10,]),
    throws_error("Error! Portfolio and commodity data should be of same length: 253 vs. 10")
  )

  expect_that(
    computeAbnormalReturn(portfolio=d.DAX[,2:7], commodity=d.VW),
    throws_error("Error! Portfolio data does not contain attribute 'Date'")
  )

  expect_that(
    computeAbnormalReturn(portfolio=d.DAX, commodity=d.VW[,2:7]),
    throws_error("Error! Commodity data does not contain attribute 'Date'")
  )

  expect_that(
    computeAbnormalReturn(portfolio=d.DAX, commodity=d.VW, attributeOfInterest = "xyz"),
    throws_error("Error! Portfolio data does not contain attribute 'xyz'")
  )

  expect_that(
    computeAbnormalReturn(portfolio=d.DAX, commodity=d.VW[,1:3]),
    throws_error("Error! Commodity data does not contain attribute 'Close'")
  )

  expect_that(
    computeAbnormalReturn(portfolio=d.DAX[,1:3], commodity=d.VW),
    throws_error("Error! Portfolio data does not contain attribute 'Close'")
  )

  d.DAX_mod = d.DAX
  d.DAX_mod$Date[2] = "2015-01-04 CET"
  expect_that(
    computeAbnormalReturn(portfolio=d.DAX_mod, commodity=d.VW),
    throws_error("Error! Dates of portfolio and commodity data sets do not match")
  )
  remove(d.DAX_mod)
})

test_that("parameter estimationWindowLength is set correct", {
  expect_that(
    computeAbnormalReturn(portfolio=d.DAX, commodity=d.VW, estimationWindowLength=2),
    throws_error("Error! A minimum estimation window size of 3")
  )

  expect_that(
    computeAbnormalReturn(portfolio=d.DAX, commodity=d.VW, estimationWindowLength=-20),
    throws_error("Error! A minimum estimation window size of 3")
  )

  expect_that(
    computeAbnormalReturn(portfolio=d.DAX, commodity=d.VW, eventIndex = 20, estimationWindowLength = 20),
    throws_error("Error! Chosen eventIndex overlaps with estimationWindow.")
  )
})

test_that("error thrown, when bad regressionType specified", {
  expect_that(
    computeAbnormalReturn(portfolio=d.DAX, commodity=d.VW, regressionType = 'xyz'),
    throws_error("Error! Unknown regressionType specified: xyz")
  )
})

test_that("plot is generated, if intended", {

  expect_null(dev.list()["RStudioGD"])
  x <- computeAbnormalReturn(portfolio=d.DAX, commodity=d.VW, showPlot=TRUE)
  expect_gt(dev.list()["RStudioGD"], 0)

  dev.off(dev.list()["RStudioGD"])
  expect_null(dev.list()["RStudioGD"])
  x <- computeAbnormalReturn(portfolio=d.DAX, commodity=d.VW, showPlot=FALSE)
  expect_null(dev.list()["RStudioGD"])

})
