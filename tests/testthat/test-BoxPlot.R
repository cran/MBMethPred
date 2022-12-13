test_that("MakeBoxPlot returns correct type", {
  data <- Data2[1:20,]
  data <- data %>%
    t() %>%
    data.frame()
  data <- cbind(rownames(data), data)
  colnames(data)[1] <- "ID"
  expect_type(BoxPlot(File = data), "list")
})
