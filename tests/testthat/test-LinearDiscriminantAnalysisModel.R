test_that("LinearDiscriminantAnalysisModel returns correct class.", {
  expect_type(LinearDiscriminantAnalysisModel(SplitRatio = 0.8, CV = 10, NCores = 1, NewData = NULL), "list")
})
