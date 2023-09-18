test_that("NaiveBayesModel returns correct class.", {
  expect_type(NaiveBayesModel(SplitRatio = 0.8, CV = 2, Threshold = 0.8 , NCores = 1, NewData = NULL), "list")
})
