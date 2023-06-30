#' @title New data prediction result
#' @name NewDataPredictionResult
#' @description A function to output the predicted medulloblastoma subgroups by trained models.
#' @export NewDataPredictionResult
#' @importFrom stringr str_extract %>%
#' @importFrom stats na.omit
#' @param Model A trained model
#' @return A data frame
#' @examples
#' set.seed(10)
#' fac <- ncol(Data1)
#' NewData <- sample(data.frame(t(Data1[,-fac])),10)
#' NewData <- cbind(rownames(NewData), NewData)
#' colnames(NewData)[1] <- "ID"
#' xgboost <- XGBoostModel(SplitRatio = 0.2,
#'                         CV = 2,
#'                         NCores = 1,
#'                         NewData = NewData)
#' NewDataPredictionResult(Model = xgboost)

NewDataPredictionResult <- function(Model){
  Mode <- function(x) {
    ux <- unique(x)
    return(ux[which.max(tabulate(match(x, ux)))])
  }
  if(!is.list(Model)){
    stop('Please provide the model.')
  }

  model_type <- str_extract(names(Model)[1], "Fold.*") %>%
    na.omit() %>%
    length()

  if(model_type != 0) {
  prediction <- list()
  for(i in seq_along(Model)) {
    prediction[[names(Model)[i]]] <- Model[[i]][["pnewdata"]]
  }
  } else{
    prediction <- list(pnewdata = Model[["pnewdata"]])
  }
  folds_predicted <- data.frame(Mode(prediction)[[1]])
  colnames(folds_predicted) <- "Subgroup"
  return(folds_predicted)
}
