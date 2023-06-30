#' @title Input file for prediction
#' @description A function to read DNA methylation files. It can be used as the new data for prediction by every model.
#' @export
#' @importFrom stringr str_extract %>%
#' @param File A data frame with tsv or csv file extension. The first column of the data frame is the CpG methylation probe that starts with cg characters and is followed by a number (e.g., cg100091). Other columns are samples with methylation beta values. All columns in the data frame should have a name.
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


