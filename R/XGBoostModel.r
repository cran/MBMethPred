#' @title XGBoost model
#' @name XGBoostModel
#' @description A function to train an XGBoost model to classify medulloblastoma subgroups using the DNA methylation dataset (Illumina Infinium HumanMethylation450). Prediction is followed by training if new data is provided.
#' @export
#' @importFrom caTools sample.split
#' @importFrom caret createFolds
#' @importFrom parallel mclapply
#' @importFrom stats predict
#' @importFrom stringr %>%
#' @param SplitRatio Train and test split ratio. A value greater or equal to zero and less than one.
#' @param CV The number of folds for cross-validation. It should be greater than one.
#' @param NCores The number of cores for parallel computing.
#' @param NewData A methylation data from the ReadMethylFile function.
#' @return A list
#' @examples
#' set.seed(123)
#' xgboost <- XGBoostModel(SplitRatio = 0.6,
#'                         CV = 2,
#'                         NCores = 1,
#'                         NewData = NULL)
load("data/Data1.RData")
XGBoostModel <- function(SplitRatio = 0.8,
                         CV = 10,
                         NCores = 1,
                         NewData = NULL){
  
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    stop("Package 'xgboost' required but not installed.")
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
        data.frame()
    }
  }
  fac <- ncol(Data1)
  split <- sample.split(Data1[, fac], SplitRatio = SplitRatio)
  training_set <- subset(Data1, split == TRUE)
  test_set <- subset(Data1, split == FALSE)
  folds <- createFolds(Data1[,fac] , CV)
  cv <- mclapply(folds, function(x){
    training_fold <- training_set[-x, ]
    test_fold <- test_set[-x, ]

    train_label <- factor(training_fold[,fac], levels = c("Group3", "Group4", "SHH","WNT"),
                          labels = c(0:3))
    train_label <- as.numeric(as.character(train_label))
    train_data <- as.matrix(training_fold[,-fac])
    train_data <- train_data[, order(colnames(train_data))]
    test_label <- factor(test_fold[,fac], levels = c("Group3", "Group4", "SHH","WNT"),
                         labels = c(0:3))
    test_label <- as.numeric(as.character(test_label))
    test_data <- as.matrix(test_fold[,-fac])
    test_data <- test_data[, order(colnames(test_data))]
    model <- xgboost::xgboost(data = train_data,
                              label = train_label,
                              nrounds = 10,
                              verbose = 1,
                              early_stopping_rounds = 10,
                              params = list(max_depth = 8,
                                            lambda = 0.01,
                                            eta = 0.7),
                              nthread = 10,
                              objective = "multi:softmax" ,
                              num_class = 4)
    y_pred <- predict(model, test_data)
    result <- ConfusionMatrix(test_label, y_pred)
    rownames(result) <- c("Group3", "Group4", "SHH","WNT")
    if(!is.null(NewData)) {
      NewData <- as.matrix(NewData)
      NewData <- NewData[, order(colnames(NewData))]
      y_pred_NewData <- predict(model, newdata = NewData)
      y_pred_NewData <- factor(y_pred_NewData, levels = c(0:3),
                               labels = c("Group3", "Group4", "SHH","WNT"))
      names(y_pred_NewData) <- rownames(NewData)
    } else {
      y_pred_NewData <- NULL
    }
    allresult <- list(result = result, pnewdata = y_pred_NewData)
    return(allresult)
  }, mc.cores = NCores)
}

