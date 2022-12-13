#' @title Artificial neural network model
#' @name NeuralNetworkModel
#' @description A function to train an artificial neural network model to classify medulloblastoma subgroups using the DNA methylation dataset (Illumina Infinium HumanMethylation450). Prediction is followed by training if new data is provided.
#' @export
#' @importFrom keras to_categorical keras_model_sequential layer_dense layer_activation_leaky_relu layer_dropout optimizer_sgd callback_early_stopping compile fit regularizer_l2
#' @importFrom stats predict
#' @importFrom stringr %>%
#' @param Epochs The number of epochs.
#' @param NewData A methylation data from ReadMethylFile function.
#' @param InstallTensorFlow Logical. Running this function for the first time, you need to install TensorFlow library (V 2.10-cpu). Default is TRUE.
#' @return A list
#' @examples
#' \dontrun{
#' set.seed(1234)
#' ann <- NeuralNetworkModel(Epochs = 100,
#'                           NewData = NULL,
#'                           InstallTensorFlow = TRUE)
#'}
load("data/Data1.RData")
NeuralNetworkModel <- function(Epochs = 100,
                               NewData = NULL,
                               InstallTensorFlow = TRUE){
  
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' required but not installed.") 
  }
  if (!requireNamespace("tensorflow", quietly = TRUE)) {
    stop("Package 'tensorflow' required but not installed.")
  }
  
  if(InstallTensorFlow) {
    old <- options()
    on.exit(options(old))
    options(timeout = 1000)
    reticulate::install_python()
    reticulate::virtualenv_create("MBMethPred-tensorflow", reticulate::install_python())
    tensorflow::install_tensorflow(envname = "MBMethPred-tensorflow", version = "2.10-cpu", restart_session = FALSE)
  }
  reticulate::use_python(reticulate::virtualenv_python("MBMethPred-tensorflow"), required = TRUE)

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
  subgroup <- data.frame(subgroup = Data1$subgroup)
  subgroup$subgroup <- factor(subgroup$subgroup, levels = c("Group3", "Group4", "SHH","WNT"),
                              labels = c(0:3))
  df_ann <- data.matrix(Data1[,-fac])
  ind <- sample(3, nrow(df_ann), replace = T, prob = c(.6, .2, .2))
  X_train <- df_ann[ind==1,1:fac-1]
  X_test <- df_ann[ind==2, 1:fac-1]
  X_val <- df_ann[ind==3, 1:fac-1]
  trainingtarget <- subgroup[ind==1,1]
  testtarget <- subgroup[ind==2, 1]
  valtarget <- subgroup[ind==3, 1]
  X_train <- X_train[, order(colnames(X_train))]
  X_test <- X_test[, order(colnames(X_test))]
  X_val <- X_val[, order(colnames(X_val))]
  y_train <- to_categorical(trainingtarget)
  y_test <- to_categorical(testtarget)
  y_val <- to_categorical(valtarget)
  num_labels <- ncol(y_train)
  model <- keras_model_sequential()
  model %>%
    layer_dense(units = 30, input_shape = ncol(X_train)) %>%
    layer_activation_leaky_relu() %>%
    layer_dropout(0.4) %>%
    layer_dense(units = 20, kernel_regularizer = regularizer_l2(0.02)) %>%
    layer_activation_leaky_relu() %>%
    layer_dropout(0.3) %>%
    layer_dense(units = 10) %>%
    layer_activation_leaky_relu() %>%
    layer_dropout(0.1) %>%
    layer_dense(units = num_labels, activation = 'softmax')
  model %>% compile(loss = 'categorical_crossentropy',
                    optimizer_sgd(learning_rate = 0.03, nesterov = TRUE,
                                  decay=1e-6, momentum = 0.05),
                    metrics = c('accuracy'))
  early_stop <- callback_early_stopping(monitor = "val_loss", patience = 5, mode = "min")
  history <- model %>%
    fit(X_train,
        y_train, verbose = 1,
        epochs = Epochs,
        batch_size = 16,
        callbacks = list(early_stop),
        validation_data = list(X_val, y_val)
    )

  y_pred <- model %>% predict(X_test)
  y_pred <- format(round(y_pred, 2), nsamll = 4)
  y_pred <- matrix(as.numeric(as.character(y_pred)), ncol = 4)
  y_pred <- max.col(y_pred)
  y_pred <- factor(y_pred, levels = c(1:4),
                   labels = c("Group3", "Group4", "SHH","WNT"))
  y_test <- max.col(y_test)
  y_test <- factor(y_test, levels = c(1:4),
                   labels = c("Group3", "Group4", "SHH","WNT"))
  result <- ConfusionMatrix(y_test, y_pred)
  if(!is.null(NewData)) {
    y_pred_NewData <- model %>% predict(NewData)
    y_pred_NewData <- format(round(y_pred_NewData, 2), nsamll = 4)
    y_pred_NewData <- max.col(y_pred_NewData)
    y_pred_NewData <- factor(y_pred_NewData, levels = c(1:4),
                             labels = c("Group3", "Group4", "SHH","WNT"))
    names(y_pred_NewData) <- rownames(NewData)
  } else {
    y_pred_NewData <- NULL
  }
  allresult <- list(result = result, pnewdata = y_pred_NewData)
  return(allresult)
}
