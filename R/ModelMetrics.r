#' @title Model metrics
#' @description A function to extract the confusion matrix information.
#' @export ModelMetrics
#' @importFrom stringr str_extract %>%
#' @importFrom stats na.omit
#' @param Model A trained model.
#' @return A list
#' @examples
#' xgboost <- XGBoostModel(SplitRatio = 0.2,
#'                         CV = 2,
#'                         NCores = 1,
#'                         NewData = NULL)
#' ModelMetrics(Model = xgboost)

ModelMetrics <- function(Model) {
  
  AverageCV <- function(results){
    mlm <- do.call(cbind, results)
    colnames(mlm) <- gsub("Fold[0-9]{1,2}.", "", colnames(mlm))
    acc <- list()
    for(i in 1:6){
      name <- unique(colnames(mlm))[i]
      num <- grep(name, colnames(mlm))
      acc[[i]] <- rowMeans(mlm[,num])
      
    }
    result <- round(do.call(cbind, acc),3)
    colnames(result) <- unique(colnames(mlm))
    return(result)
  }
  
  Mode <- function(x) {
    ux <- unique(x)
    return(ux[which.max(tabulate(match(x, ux)))][[1]])
  }
  
  if(!is.list(Model)) {
    stop('Please provide the model.')
  }
  model_type <- str_extract(names(Model)[1], "Fold.*") %>%
    na.omit() %>%
    length()
  
  if(model_type != 0) {
    confusionmat <- list()  
    results <- list()
    for(i in seq_along(Model)) {
      results[[names(Model)[i]]] <- Model[[i]][["result"]]
      confusionmat[[names(Model)[i]]] <- Model[[i]][["ConfusionMat"]]
    }
    return(list(ConfusionMatrix = Mode(confusionmat), ModelPerformance = AverageCV(results)))
  } else {
    return(list(ConfusionMatrix = Model[["ConfusionMat"]], ModelPerformance = Model[["result"]]))
  }
}
