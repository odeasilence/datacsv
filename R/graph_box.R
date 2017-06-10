#' graph_box
#'
#' This is a function that will generate a box graph containing a specified column from each csv file in the folder.
#' @param folder Input folder path
#' @param colname Input the name of the column wanted to extract
#' @param vec if a vector will be generated
#' @export
graph_box <- function(folder,colname,vec=FALSE){
  setwd(folder)
  filename <- list.files(pattern = "*.csv")
  column <- 0
  if(vec == TRUE){vect <<-0}
  for (i in 1: length(filename)) {
    column[i] <- assign(filename[i], select(read.csv(filename[i]),eval(as.name(paste(colname)))))
    #extract column wanted
  }
  min.length <- min(sapply(column,length)) #find the smallest number of row
  result <<- data.frame(matrix(ncol = 0,nrow=min.length))
  for (i in 1:length(filename)) {
    result[file_path_sans_ext(filename[i])] <<- sample(unlist(column[i]),size = min.length)
    #randomly select a number of data point
    if(vec == TRUE){vect[i] <<- as.list(result[filename[i]])}
  }
  if(vec == TRUE){vect <<- unlist(vect)}
  ggplot(stack(result), aes(x = ind, y = values))+
    geom_boxplot()+
    theme(axis.title.x = element_blank())
}
