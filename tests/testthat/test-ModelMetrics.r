test_that("ModelMetrics returns correct class.", {
  xgboost <- XGBoostModel(SplitRatio = 0.8, CV = 2, NCores = 1, NewData = NULL)
  expect_type(ModelMetrics(Model = xgboost), "list")
})
