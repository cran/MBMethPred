test_that("SupportVectorMachineModel returns correct class.", {
  expect_type(SupportVectorMachineModel(SplitRatio = 0.8, CV = 3, NCores = 1, NewData = NULL), "list")
})
