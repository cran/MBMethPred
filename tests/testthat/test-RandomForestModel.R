test_that("RandomForestModel returns correct class.", {
  expect_type(RandomForestModel(SplitRatio = 0.8, CV = 2, NTree = 10, NCores = 1, NewData = NULL), "list")
})
