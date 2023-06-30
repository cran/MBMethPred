#' @title K nearest neighbor model
#' @name KNearestNeighborModel
#' @description A function to train a K nearest neighbor model to classify medulloblastoma subgroups using DNA methylation beta values (Illumina Infinium HumanMethylation450). Prediction is followed by training if new data is provided.
#' @export
#' @importFrom caTools sample.split
#' @importFrom caret createFolds
#' @importFrom parallel mclapply
#' @importFrom stats predict
#' @importFrom stringr %>%
#' @param SplitRatio Train and test split ratio. A value greater or equal to zero and less than one.
#' @param CV The number of folds for cross-validation. It should be greater than one.
#' @param K The number of nearest neighbors.
#' @param NCores The number of cores for parallel computing.
#' @param NewData A methylation beta values input from the ReadMethylFile function.
#' @return A list
#' @examples
#' set.seed(111)
#' knn <- KNearestNeighborModel(SplitRatio = 0.8,
#'                              CV = 3,
#'                              K = 3,
#'                              NCores = 1,
#'                              NewData = NULL)
load("data/Data1.RData")                             
KNearestNeighborModel <- function(SplitRatio = 0.8,
                                  CV = 10,
                                  K = 3,
                                  NCores = 1,
                                  NewData = NULL){
  
  if (!requireNamespace("class", quietly = TRUE)) {
    stop("Package 'class' required but not installed.")
  }
  if(CV <= 1) {
    stop('Please provide more than 1 cross validation folds.')
  }
  Data1$subgroup <- factor(Data1$subgroup)
  fac <- ncol(Data1)
  if(!is.null(NewData)){
    if(colnames(NewData)[1] != "ID") {
      stop('Please prodide correct NewData file.')
    } else {
      rownames(NewData) <- NewData$ID
      NewData <- NewData[,-1]
      common_mat <- which(colnames(Data1) %in% rownames(NewData))
      common_new <- which(rownames(NewData) %in% colnames(Data1)[-fac])
      Data1 <- Data1[, c(common_mat, fac)]
      NewData <- NewData[common_new, ] %>%
        t() %>%
        as.matrix()
      NewData <- NewData[, order(colnames(NewData))]
    }
  }
  fac <- ncol(Data1)
  split <- sample.split(Data1[, fac], SplitRatio = SplitRatio)
  training_set <- subset(Data1, split == TRUE)
  test_set <- subset(Data1, split == FALSE)
  training_set <- training_set[, order(colnames(training_set))]
  test_set <- test_set[, order(colnames(test_set))]
  folds <- createFolds(Data1[,fac] , CV)
  cv <- mclapply(folds, function(x){
    training_fold <- training_set[-x, ]
    test_fold <- test_set[-x, ]
    knnclass_pred <- class::knn(train = training_fold[, -fac],
                                test = test_fold[, -fac],
                                cl = training_fold[, fac],
                                k = K)
    if(!is.null(NewData)){
    knnclass_prednew <- class::knn(train = training_fold[, -fac],
                                   test = NewData,
                                   cl = training_fold[, fac],
                                   k = K)
    names(knnclass_prednew) <- rownames(NewData)
    } else {
      knnclass_prednew <- NULL
    }
    result <- ConfusionMatrix(test_fold[, fac], knnclass_pred)
    allresult <- list(result = result, pnewdata = knnclass_prednew)
    return(allresult)
  }, mc.cores = NCores)
}
