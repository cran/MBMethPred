test_that("ConfusionMatrix returns correct type", {
  set.seed(1234)
  data <- Data1[1:20,]
  fac <- ncol(data)
  data$subgroup <- factor(data$subgroup)
  split <- sample.split(data[, fac], SplitRatio = 0.8)
  training_set <- subset(data, split == TRUE)
  test_set <- subset(data, split == FALSE)
  rf <- randomForest::randomForest(x = training_set[-fac], y = training_set[, fac], ntree = 10)
  y_pred <- predict(rf, newdata = test_set[-fac])
  expect_type(ConfusionMatrix(y_true = test_set[, fac], y_pred = y_pred), "double")
})

