library(testthat)
library(W04hw)
test_that("make_filename ", {
  expect_that(make_filename(2013), is_identical_to("accident_2013.csv.bz2"))
})
