context('Test whether month_index function works')

test_that('month_index works', {

  # setup test data set
  y = c(rep(2016,4),
        rep(2017,3),
        2018)

  m = c(5, 6, 7, 8,
        3, 5, 8,
        3)

  sales = c(1,1,1,1,
            2,2,2,
            3)

  test_df = data.frame(y, m, sales)

  # setup test results
  y = c(rep(2016, 8),
        rep(2017,12),
        rep(2018, 3))

  m = c(seq(5, 12),
        seq(1, 12),
        seq(1, 3))

  month_index = seq(1, 23)

  correct_result = data.frame(y, m, month_index)

  # clean up to not risk make_month_finding vectors
  rm(y, m, sales, month_index)

  # test results
  expect_equal(mmmTools::make_month_index(df = test_df,
                                          SLS_YR = y,
                                          SLS_MNTH_INT = m),
               correct_result)

})
