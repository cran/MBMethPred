#' @title Input file for similarity network fusion (SNF)
#' @description A function to read user-provided file feeding into the SNF function (from the SNFtools package).
#' @export
#' @importFrom stringr str_extract %>%
#' @importFrom utils write.csv
#' @param File A table with tsv or csv file extension. While the first column is a character vector (e.g., gene names or IDs), columns are samples. All columns should be named.
#' @return A data frame
#' @examples
#' \dontrun{
#' data <- ReadSNFData(File = "file.csv")
#' }
ReadSNFData <- function(File) {
  
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("Package 'readr' required but not installed.")
  }
  
  path <- file.path(File)
  format <- str_extract(path, '(?=.)[a-z]{3}$')
  ex <- grepl(format, "tsv|csv")
  if(!ex){
    stop('Please provide a tsv or csv file format.')
  }
  if(format == "csv"){
    matrix <- data.frame(readr::read_csv(File, num_threads = 2))
  }
  if(format == "tsv"){
    matrix <- data.frame(readr::read_tsv(File))
  }
  dftype <- sapply(matrix, class)[1]
  if(dftype != "character"){
    stop('The first column should be character (e.g, gene names or IDs).')
  }
  else if(dftype == "character"){
    rownames(matrix) <- matrix[, 1]
    matrix <- matrix[, -1] %>%
      data.frame()
  }
}
