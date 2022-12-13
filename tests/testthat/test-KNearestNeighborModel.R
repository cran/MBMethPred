test_that("NaiveBayesModel returns correct class.", {
  expect_type(KNearestNeighborModel(SplitRatio = 0.8, CV = 3, K = 3, NCores = 1, NewData = NULL), "list")
})
