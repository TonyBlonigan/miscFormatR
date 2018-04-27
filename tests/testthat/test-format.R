context('Test whether formatting functions works')

test_that('dollarChar works', {
  #simple formatting of char
  expect_equal('$2.12M', dollarChar(x = 2118999, unit = 'm'))

  # more conversions
  x = c(2888999, 1499999)
  expect_equal(c('$3M','$1M'), dollarChar(x = x,  ndecimal = 0, unit = 'm'))

  # convert multiple numbers
  x = c(-2014,7987)
  expect_equal(dollarChar(x = x),
               c("-$2,014", "$7,987"))

})
