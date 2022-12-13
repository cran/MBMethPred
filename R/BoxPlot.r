#' @title Box plot
#' @name BoxPlot
#' @description A function to draw a box plot for the DNA methylation dataset.
#' @export
#' @import ggplot2
#' @param File The output of ReadMethylFile function.
#' @param Projname A name used to name the plot. The default is null.
#' @return A ggplot2 object
#' @examples
#' data <- Data2[1:10,]
#' data <- cbind(rownames(data), data)
#' colnames(data)[1] <- "ID"
#' BoxPlot(File = data)


BoxPlot <- function(File,
                    Projname = NULL) {

  if (!requireNamespace("reshape2", quietly = TRUE)) {
    stop("Package 'reshape2' required but not installed.")
  }
  
  if(colnames(File)[1] == "ID"){
  melted <- reshape2::melt(File, id.vars= "ID")
  my_pal <- c("#2980B9")
  ggplot(data = melted, aes(x = variable, y = value)) +
    geom_boxplot(aes(color = "#2980B9", fill= "#2980B9"), outlier.alpha = 0.1)+
    labs(x= 'SampleID', y= 'Values') +
    scale_y_continuous(labels = scales::comma) +
    theme_classic() +
    scale_color_manual(values=c(my_pal)) +
    scale_fill_manual(values=c(paste(my_pal, "66", sep = ""))) +
    theme(axis.text = element_text(size = 16 , colour = "black", angle = 90),
          axis.text.x = element_text(colour = "black", size = 6),
          axis.text.y = element_text(colour = "black", size = 12, angle = 00),
          plot.subtitle = element_text(size = 24, colour = "black", hjust = 0.5),
          axis.title.y = element_text(size = 16, angle = 90),
          axis.title.x = element_text(size = 16, angle = 00),
          legend.position="none") +
    labs(subtitle = paste0( Projname, " Box Plot"))
  } else{
    stop('Please provide a file with the first column as cg probes.')
  }
}


