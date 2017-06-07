#'loadcsv
#'
#'This function allow you to load multiple csv files
#'It will change the working directory.
#'@param folder specified path of the folder
#'@param individual A logical parameter.
#' If set TRUE, it will create individual data frames for each csv file.
#' If set FALSE, a single data frame will be created
#' with an additional column named "Source" that contains the name of csv file
#'@export
loadcsv <- function(folder,individual=FALSE){
  setwd(folder)
  temp <<- list.files(pattern = "*.csv")
  if(individual==TRUE){
  for (i in 1: length(temp)) {
    assign(temp[i], read.csv(temp[i]),envir = globalenv())
  }
  #generate individuaOl data frames
  }
  if(individual==FALSE){
  read_csv_filename <- function(filename){
    ret <- read.csv(filename)
    ret$Source <- filename
    ret
  }
  import.list <- ldply(temp, read_csv_filename)
  Result <<- do.call("rbind",import.list) %>%
    t() %>%
    as.data.frame()
  #generate single data frame
  }
}
