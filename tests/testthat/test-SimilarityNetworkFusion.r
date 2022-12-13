test_that("SimilarityNetworkFusion returns correct class.", {
  data(RLabels) # Real labels
  data(Data2) # Methylation
  data(Data3) # Gene expression
  snf <- SimilarityNetworkFusion(Files = list(Data2, Data3),
                                 NNeighbors  = 13,
                                 Sigma = 0.75,
                                 NClusters = 4,
                                 CLabels = c("Group4", "SHH", "WNT", "Group3"),
                                 RLabels = RLabels,
                                 Niterations = 10)
  snf
  expect_type(snf, "integer")
})
