#' @title Similarity network fusion (SNF)
#' @description A function to perform SNF function (from SNFtool package) and output clusters.
#' @export
#' @importFrom stats dist
#' @param Files A list of data frames created using the ReadSNFData function or matrices.
#' @param NNeighbors The number of nearest neighbors.
#' @param Sigma The variance for local model.
#' @param NClusters The number of clusters.
#' @param CLabels A string vector to name the clusters. Optional.
#' @param RLabels The actual label of samples to calculate the Normalized Mutual Information (NMI) score. Optional.
#' @param Niterations The number of iterations for the diffusion process.
#' @return Factor
#' @examples
#' data(RLabels) # Real labels
#' data(Data2) # Methylation
#' data(Data3) # Gene expression
#'snf <- SimilarityNetworkFusion(Files = list(Data2, Data3),
#'                                NNeighbors  = 13,
#'                                Sigma = 0.75,
#'                                NClusters = 4,
#'                                CLabels = c("Group4", "SHH", "WNT", "Group3"),
#'                                RLabels = RLabels,
#'                                Niterations = 10)
#' snf

SimilarityNetworkFusion <- function(Files = NULL,
                                    NNeighbors,
                                    Sigma,
                                    NClusters,
                                    CLabels = NULL,
                                    RLabels = NULL,
                                    Niterations){
  
  if (!requireNamespace("SNFtool", quietly = TRUE)) {
    stop("Package 'SNFtool' required but not installed.")
  }
  
  if(!is.list(Files)) {
    stop('Please provide a list of two or more datasets (e.g., list(data1, data2)).')
  }
  if(length(Files) > 1){
    coln_list <- list()
    for(i in seq_along(Files)){
      coln_list[[i]] <- colnames(Files[[i]])
    }
    compare <- list()
    for(i in seq_along(coln_list)){
      common <- Reduce(intersect, coln_list)
      compare[[i]] <- identical(coln_list[[i]], common)
    }
    truth <- unlist(compare)
    if(FALSE %in% truth){
      stop("Please make sure that sample names from all files are the same.")
    }
    Wall_list <- list()
    for(i in seq_along(Files)){
      distm <- as.matrix(dist(t(Files[[i]])))
      Wall <- SNFtool::affinityMatrix(distm, K = NNeighbors, sigma = Sigma)
      Wall_list[[i]] <- Wall
    }
    W <- SNFtool::SNF(Wall = Wall_list, K = NNeighbors, t = Niterations)
    SNFtool::displayClustersWithHeatmap(W, SNFtool::spectralClustering(W, K = NClusters))
    SC <- SNFtool::spectralClustering(W, K = NClusters)
    if(!is.null(CLabels)){
      SC <- factor(SC,
                   levels = 1:NClusters,
                   labels = CLabels)
    }
    if(!is.null(RLabels)){
      NMI <- SNFtool::calNMI(SC, RLabels)
      message("The NMI score is: ", NMI)
    }
    return(SC)
  }
}
