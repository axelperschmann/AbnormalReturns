library(testthat)
context("Abnormal returns")

test_that("function does not throw any errors or warnings if called with default settings", {
  expect_silent(abnormalReturn(prices_market = d.DAX, prices_stock = d.VW))
  expect_silent(abnormalReturn(prices_market = d.DAX, prices_stock = d.VW, showPlot = TRUE))
})

test_that("returned data frame has correct dimensions", {
  windowLength = 20
  x <- abnormalReturn(prices_market = d.DAX, prices_stock = d.VW,
                             estimationWindowLength=windowLength)
  expect_equal(nrow(x), nrow(d.DAX) - windowLength)
  expect_equal(names(x), c("Date", "abnormalReturn", "cumulativeAbnormalReturn", "stockReturn"))
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
  expect_error(abnormalReturn(prices_market = d.DAX, prices_stock = d.VW, estimationWindowLength = 2))

  expect_error(abnormalReturn(prices_market = d.DAX, prices_stock = d.VW, estimationWindowLength = -20))

  expect_error(abnormalReturn(prices_market = d.DAX, prices_stock = d.VW, eventIndex = 20, estimationWindowLength = 20))
})

test_that("input checks are performed", {
  expect_error(abnormalReturn(prices_market = d.DAX, prices_stock = d.VW, estimationWindowLength = '10'))
  expect_error(abnormalReturn(prices_market = d.DAX, prices_stock = 3))
  expect_error(abnormalReturn(prices_market = c(1,3,4), prices_stock = d.VW))
  expect_error(abnormalReturn(prices_market = d.DAX, prices_stock = d.VW, showPlot=1))
  expect_error(abnormalReturn(prices_market = d.DAX, prices_stock = d.VW, attributeOfInterest=3))
  expect_error(abnormalReturn(prices_market = d.DAX, prices_stock = d.VW, c='3'))
  expect_error(abnormalReturn(prices_stock = d.VW, model = 'marketmodel'))
  expect_error(abnormalReturn(prices_stock = d.VW, model = 'unknownmodel'))
  expect_silent(abnormalReturn(prices_stock = d.VW, model = 'constantmeanmodel'))

  expect_error(abnormalReturn(prices_stock="VOW3.DE", model = 'constantmeanmodel'))
  expect_silent(abnormalReturn(prices_stock="VOW3.DE", model = 'constantmeanmodel', from='2015-01-01', to='2015-12-31'))
  expect_error(abnormalReturn(prices_stock="VOW3.DE", prices_market="%5EGDAXI", model='marketmodel'))
  expect_silent(abnormalReturn(prices_stock="VOW3.DE", prices_market="%5EGDAXI", model='marketmodel', from='2015-01-01', to='2015-12-31'))
  expect_error(abnormalReturn(prices_stock="VOW3.DE", model = 'constantmeanmodel', from='2016-01-01', to='1993-12-31'))
})

