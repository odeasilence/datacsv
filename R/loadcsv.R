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
  filenames <<- list.files(pattern = "*.csv")
  if(individual==TRUE){
  for (i in 1: length(filenames)) {
    assign(file_path_sans_ext(filenames[i]), read.csv(filenames[i]),envir = globalenv())
  }
  #generate individuaOl data frames
  }
  if(individual==FALSE){
  read_csv_filename <- function(filename){
    ret <- read.csv(filename)
    ret$Source <- file_path_sans_ext(filename)
    ret
  }
  import.list <- ldply(filenames, read_csv_filename)
  Result <<- do.call("rbind",import.list) %>%
    t() %>%
    as.data.frame()
  #generate single data frame
  Result[,1:length(filenames)] <<- as.numeric(as.character(unlist(Result[,1:length(temp)])))
  Result$Source <<- as.character((unlist(Result$Source)))
  return(Result)
  }
}
