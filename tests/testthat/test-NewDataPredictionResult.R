test_that("NewDataPredictionResult returns correct type", {
  set.seed(1234)
  fac <- ncol(Data1)
  NewData <- Data1[,-fac] %>%
    t() %>%
    data.frame() %>%
    sample(10)
  NewData <- cbind(rownames(NewData), NewData)
  colnames(NewData)[1] <- "ID"
  xgboost <- XGBoostModel(SplitRatio = 0.8, CV = 5, NCores = 1, NewData = NewData)
  expect_type(NewDataPredictionResult(Model = xgboost), "list")
})
