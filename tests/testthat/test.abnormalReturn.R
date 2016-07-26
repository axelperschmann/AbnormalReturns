library(testthat)
context("Abnormal returns")

test_that("function does not throw any errors or warnings if called with default settings", {
  expect_silent(abnormalReturn(prices_market = d.DAX, prices_stock = d.VW))
  expect_silent(abnormalReturn(prices_market = d.DAX, prices_stock = d.VW, showPlot = TRUE))
})

test_that("returned data frame has correct dimensions (eventIndex==NULL)", {
  windowLength = 20
  x <- abnormalReturn(prices_market = d.DAX, prices_stock = d.VW, eventIndex=NULL,
                             estimationWindowLength=windowLength)
  expect_equal(nrow(x), nrow(d.DAX) - windowLength)
  expect_equal(names(x), c("Date", "abnormalReturn", "R.squared", "stockReturn", "marketReturn"))
})

test_that("returned data frame has correct dimensions (eventIndex!=NULL)", {
  x <- abnormalReturn(prices_market = d.DAX, prices_stock = d.VW, eventIndex=50)
  expect_equal(nrow(x), 1)
  expect_equal(names(x), c("Date", "abnormalReturn", "R.squared", "stockReturn", "marketReturn"))
})

test_that("portfolio and commodity are checked for conformity", {
  expect_error(abnormalReturn(prices_market = d.DAX[1:252,], prices_stock = d.VW))

  expect_error(abnormalReturn(prices_market = d.DAX, prices_stock = d.VW[1:10,]))

  expect_error(abnormalReturn(prices_market = d.DAX[,2:7], prices_stock = d.VW))

  expect_error(abnormalReturn(prices_market = d.DAX, prices_stock = d.VW[,2:7]))

  expect_error(abnormalReturn(prices_market = d.DAX, prices_stock = d.VW, attributeOfInterest = "xyz"))

  expect_error(abnormalReturn(prices_market = d.DAX, prices_stock = d.VW[,1:3]))

  expect_error(abnormalReturn(prices_market = d.DAX[,1:3], prices_stock = d.VW))

  d.DAX_mod = d.DAX
  d.DAX_mod$Date[2] = "2015-01-04 CET"
  expect_error(abnormalReturn(prices_market = d.DAX_mod, prices_stock = d.VW))
  remove(d.DAX_mod)
})

test_that("parameter estimationWindowLength is set correct", {
  expect_error(abnormalReturn(prices_market = d.DAX, prices_stock = d.VW, estimationWindowLength = 2),)

  expect_error(abnormalReturn(prices_market = d.DAX, prices_stock = d.VW, estimationWindowLength = -20))

  expect_error(abnormalReturn(prices_market = d.DAX, prices_stock = d.VW, eventIndex = 20, estimationWindowLength = 20))
})

test_that("error thrown, when bad regressionType specified", {
  expect_error(abnormalReturn(prices_market = d.DAX, prices_stock = d.VW, regressionType = 'xyz'))
})
