


complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  ## Test data
  ##directory <- "specdata"
  ##id <- 1
  full_dir <- paste0("./", directory ,"/")
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  xid <- function(id) {
    if (id < 10) { 
      paste0(full_dir,0,0,id,".csv")
    } else if (id < 100) {
      paste0(full_dir,0,id,".csv")
    } else paste0(full_dir,id,".csv")
    
  }
  xfile <- lapply(id,xid) 
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  nm <- c("ID","nobs")
  reportout <- as.data.frame(matrix(nrow = 0, ncol = 2,dimnames = list(NULL, nm)))
  data <- do.call(rbind,lapply(xfile,read.csv))
  bothbad <-complete.cases(data$sulfate,data$nitrte)
  ccase <- sum(bothbad)
  report <- c(id,ccase)
  #dim(report) <-c(1,2)
 
  
  ## Report function
  reportout <- c(report)
  
  return(reportout)
  
}