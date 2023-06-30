#' @title t-SNE 3D plot
#' @name TSNEPlot
#' @description A function to draw a 3D t-SNE plot for DNA methylation beta values using the K-means clustering technique.
#' @export
#' @importFrom stats kmeans
#' @importFrom stringr %>%
#' @importFrom stats na.omit
#' @param File The output of ReadMethylFile function.
#' @param NCluster The number of cluster.
#' @return Objects of rgl 
#' @examples
#' \donttest{
#' set.seed(123)
#' data <- Data2[1:100,]
#' data <- data.frame(t(data))
#' data <- cbind(rownames(data), data)
#' colnames(data)[1] <- "ID"
#' TSNEPlot(File = data, NCluster = 4)
#' }

TSNEPlot <- function(File, NCluster = 4){
  
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' required but not installed.")
  }
  if (!requireNamespace("Rtsne", quietly = TRUE)) {
    stop("Package 'Rtsne' required but not installed.")
  }
  if (!requireNamespace("rgl", quietly = TRUE)) {
    stop("Package 'rgl' required but not installed.")
  }
  if(colnames(File)[1] == "ID"){
    rownames(File) <- File[,1]
    File <- File[,-1] %>%
      na.omit() %>%
      t() %>%
      as.matrix()
  } else {
    File <- File %>%
      na.omit() %>%
      t() %>%
      as.matrix()
  }
  tSNE_fit <- File %>%
    scale() %>%
    Rtsne::Rtsne(dims = 3)

  tSNE_df <- tSNE_fit$Y %>%
    as.data.frame() %>%
    dplyr::rename(tSNE1="V1",
                  tSNE2="V2",
                  tSNE3="V3")
  clust <- kmeans(tSNE_df, NCluster)$cluster %>%
    as.factor()
  tSNE_df <- tSNE_df %>%
    dplyr::mutate(Clusters = clust)
  my_pal <-  c("#FF0000", "#00FF00", 
               "#0000FF", "#FFFF00",
               "#FF00FF", "#00FFFF",
               "#800000", "#008000",
               "#000080", "#808000",
               "#800080", "#008080", 
               "#C0C0C0", "#808080", 
               "#FF8000", "#800000",
               "#FF00FF", "#FFFF00", 
               "#008000", "#00FF00",
               "#0000FF", "#800080", 
               "#FF8000", "#FF0000",
               "#000000")
  p <-  rgl::plot3d(
    x=tSNE_df$tSNE1, y=tSNE_df$tSNE2, z=tSNE_df$tSNE3,
    col = my_pal[tSNE_df$Clusters],
    type = 's',
    radius = 1,
    size = 1.5,
    xlab="tSNE1", ylab="tSNE2", zlab="tSNE3")
  rgl::legend3d("topright",
                legend = levels(factor(tSNE_df$Clusters)),
                title = "Clusters",
                pch = 16,
                col = my_pal,
                cex= 1,
                inset=c(0.01))
  return(p)

}
