% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loadcsv.R
\name{loadcsv}
\alias{loadcsv}
\title{loadcsv}
\usage{
loadcsv(folder, individual = FALSE)
}
\arguments{
\item{folder}{specified path of the folder}

\item{individual}{A logical parameter.
If set TRUE, it will create individual data frames for each csv file. 
If set FALSE, a single data frame will be created 
with an additional column named "Source" that contains the name of csv file}
}
\description{
This function allow you to load multiple csv files
It will change the working directory.
}
