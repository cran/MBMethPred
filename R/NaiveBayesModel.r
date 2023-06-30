#' @title Naive bayes model
#' @name NaiveBayesModel
#' @description A function to train a Naive Bayes model to classify medulloblastoma subgroups using DNA methylation beta values (Illumina Infinium HumanMethylation450). Prediction is followed by training if new data is provided.
#' @export
#' @importFrom caTools sample.split
#' @importFrom caret createFolds
#' @importFrom parallel mclapply
#' @importFrom stats predict as.formula
#' @importFrom stringr %>%
#' @param SplitRatio Train and test split ratio. A value greater or equal to zero and less than one.
#' @param CV The number of folds for cross-validation. It should be greater than one.
#' @param Threshold The threshold for deciding class probability. A value greater or equal to zero and less than one.
#' @param NCores The number of cores for parallel computing.
#' @param NewData A methylation beta values input from the ReadMethylFile function.
#' @return A list
#' @examples
#' set.seed(123)
#' nb <- NaiveBayesModel(SplitRatio = 0.8,
#'                       CV = 2,
#'                       Threshold = 0.8,
#'                       NCores = 1,
#'                       NewData = NULL)
load("data/Data1.RData")
NaiveBayesModel <- function(SplitRatio = 0.8,
                            CV = 10,
                            Threshold = 0.8,
                            NCores = 1,
                            NewData = NULL) {
  
  if (!requireNamespace("e1071", quietly = TRUE)) {
    stop("Package 'e1071' required but not installed.")
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
    formula <- as.formula(paste0(names(Data1)[fac], " ~ ."))
    classifier <- e1071::naiveBayes(formula,
                                    data = training_fold,
                                    na.action = na.omit)
    y_pred <- predict(classifier, newdata = test_fold[-fac], threshold = Threshold)
    result <- ConfusionMatrix(test_fold[, fac], y_pred)
    if(!is.null(NewData)) {
      y_pred_NewData <- predict(classifier, newdata = NewData, threshold = Threshold)
      names(y_pred_NewData) <- rownames(NewData)
    } else {
      y_pred_NewData <- NULL
    }
    allresult <- list(result = result, pnewdata = y_pred_NewData)
    return(allresult)
  }, mc.cores = NCores)
}
