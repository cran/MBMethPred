#' @title Input file for prediction
#' @description A function to read DNA methylation files can be used as the new data for prediction by every model.
#' @export
#' @importFrom stringr str_extract %>%
#' @param File A data frame with tsv or csv file extension. While the first column is CpG methylation probs, starting with cg and followed by a number, other columns are samples with methylation values. All columns should be named.
#' @return A data frame
#' @examples
#' \dontrun{
#' methyl <- ReadMethylFile(File = "file.csv")
#' }

ReadMethylFile <- function(File) {
  
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
    matrix <- data.frame(readr::read_tsv(File, num_threads = 2))
  }
  cg_row <- str_extract(matrix[2,1], "cg.*")
  if(!is.na(cg_row)){
    colnames(matrix)[1] <- "ID"
    matrix <- matrix %>%
      na.omit()
  } else {
    stop('Please provide a file with the first column as cg probes.')
  }
}


