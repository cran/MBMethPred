test_that("XGBoostModel returns correct class.", {
  expect_type(XGBoostModel(SplitRatio = 0.8, CV = 2, NCores = 1, NewData = NULL), "list")
})
