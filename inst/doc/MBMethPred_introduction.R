## ---- include = FALSE---------------------------------------------------------

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo = TRUE,
	fig.width = 6,
	message = FALSE,
	warning = FALSE
)

## ----setup, include = FALSE---------------------------------------------------
require(MBMethPred)

## -----------------------------------------------------------------------------
# set.seed(1234)
# fac <- ncol(Data1)
# NewData <- sample(data.frame(t(Data1[,-fac])),10)
# NewData <- cbind(rownames(NewData), NewData)
# colnames(NewData)[1] <- "ID"
# write.csv(NewData, "NewData.csv", quote = FALSE, row.names = FALSE)
# methyl <- ReadMethylFile(File = "NewData.csv")

## ----fig.width= 8, fig.height=5-----------------------------------------------

data <- Data2[1:20,]
data <- cbind(rownames(data), data)
colnames(data)[1] <- "ID"
BoxPlot(File = data, Projname = NULL)

## -----------------------------------------------------------------------------
data <- data.frame(t(Data2[1:100,]))
data <- cbind(rownames(data), data)
colnames(data)[1] <- "ID"
TSNEPlot(File = data, NCluster = 4)

## -----------------------------------------------------------------------------
# rgl.snapshot('tsne3d.png', fmt = 'png')

## -----------------------------------------------------------------------------
# data(Data2) # Gene expression 
# Data2 <- cbind(rownames(Data2), Data2)
# colnames(Data2)[1] <- "ID"
# write.csv(Data2, "Data2.csv", row.names = FALSE)
# Data2 <- ReadSNFData(File = "Data2.csv")

## -----------------------------------------------------------------------------
data(RLabels) # Real labels
data(Data2) # Methylation
data(Data3) # Gene expression
snf <- SimilarityNetworkFusion(Files = list(Data2, Data3),
                               NNeighbors  = 13,
                               Sigma = 0.75,
                               NClusters = 4,
                               CLabels = c("Group4", "SHH", "WNT", "Group3"),
                               RLabels = RLabels,
                               Niterations = 60)
snf

## -----------------------------------------------------------------------------
set.seed(1234)
fac <- ncol(Data1)
NewData <- sample(data.frame(t(Data1[,-fac])),10)
NewData <- cbind(rownames(NewData), NewData)
colnames(NewData)[1] <- "ID"

svm <- SupportVectorMachineModel(SplitRatio = 0.8, 
                                 CV = 10, 
                                 NCores = 1, 
                                 NewData = NewData)
ModelMetrics(Model = svm)
NewDataPredictionResult(Model = svm)

## -----------------------------------------------------------------------------
set.seed(1234)
fac <- ncol(Data1)
NewData <- sample(data.frame(t(Data1[,-fac])),10)
NewData <- cbind(rownames(NewData), NewData)
colnames(NewData)[1] <- "ID"

knn <- KNearestNeighborModel(SplitRatio = 0.8, 
                             CV = 10, 
                             K = 3, 
                             NCores = 1, 
                             NewData = NewData)
ModelMetrics(Model = knn)
NewDataPredictionResult(Model = knn)

## -----------------------------------------------------------------------------
set.seed(1234)
fac <- ncol(Data1)
NewData <- sample(data.frame(t(Data1[,-fac])),10)
NewData <- cbind(rownames(NewData), NewData)
colnames(NewData)[1] <- "ID"

rf <- RandomForestModel(SplitRatio = 0.8, 
                        CV = 10, 
                        NTree = 100, 
                        NCores = 1, 
                        NewData = NewData)
ModelMetrics(Model = rf)
NewDataPredictionResult(Model = rf)

## -----------------------------------------------------------------------------
set.seed(1234)
fac <- ncol(Data1)
NewData <- sample(data.frame(t(Data1[,-fac])),10)
NewData <- cbind(rownames(NewData), NewData)
colnames(NewData)[1] <- "ID"

xgboost <- XGBoostModel(SplitRatio = 0.8, 
                        CV = 10, 
                        NCores = 1, 
                        NewData = NewData)
ModelMetrics(Model = xgboost)
NewDataPredictionResult(Model = xgboost)

## -----------------------------------------------------------------------------
set.seed(1234)
fac <- ncol(Data1)
NewData <- sample(data.frame(t(Data1[,-fac])),10)
NewData <- cbind(rownames(NewData), NewData)
colnames(NewData)[1] <- "ID"

lda <- LinearDiscriminantAnalysisModel(SplitRatio = 0.8, 
                                       CV = 10, 
                                       NCores = 1, 
                                       NewData = NewData)
ModelMetrics(Model = lda)
NewDataPredictionResult(Model = lda)

## -----------------------------------------------------------------------------
set.seed(1234)
fac <- ncol(Data1)
NewData <- sample(data.frame(t(Data1[,-fac])),10)
NewData <- cbind(rownames(NewData), NewData)
colnames(NewData)[1] <- "ID"

nb <- NaiveBayesModel(SplitRatio = 0.8, 
                      CV = 10, 
                      Threshold = 0.8, 
                      NCores = 1, 
                      NewData = NewData)
ModelMetrics(Model = nb)
NewDataPredictionResult(Model = nb)

## -----------------------------------------------------------------------------
# set.seed(1234)
# fac <- ncol(Data1)
# NewData <- sample(data.frame(t(Data1[,-fac])),10)
# NewData <- cbind(rownames(NewData), NewData)
# colnames(NewData)[1] <- "ID"
# ann <- NeuralNetworkModel(Epochs = 100, 
#                           NewData = NewData,
#                           InstallTensorFlow = TRUE)
# ModelMetrics(Model = ann)
# NewDataPredictionResult(Model = ann)

