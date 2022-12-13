test_that("TSNEPlot returns correct type", {
  data <- Data2[1:100,]
  data <- data %>%
    t() %>%
    data.frame()
  data <- cbind(rownames(data), data)
  colnames(data)[1] <- "ID"
  expect_type(TSNEPlot(File = data, NCluster = 4), "double")
})
