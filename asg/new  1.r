

 ### Program 1 for Week 2

pollutantmean <- function(directory,pollutant,id){

  ## test values
  #directory <- "specdata"
  #pollutant <- "nitrate"
  #id <- 70:72
  full_dir <- paste0("./", directory ,"/")
  
  ## link the id to the filename
  
  xid <- function(id) {
    if (id < 10) { 
      paste0(full_dir,0,0,id,".csv")
    } else if (id < 100) {
      paste0(full_dir,0,id,".csv")
    } else paste0(full_dir,id,".csv")
   
  }
  
  xfile <- lapply(id,xid)
  
  ## loads the csv file into a matrix
  
  activedata <- do.call(rbind,lapply(xfile,read.csv))
  
  ## set the pollutant search
  
  if (pollutant == "sulfate") {
    xpart <- activedata$sulfate
  } else if (pollutant == "nitrate") {
    xpart <- activedata$nitrate
  } else {
    print ("Please enter either sulfate or nitrate for pollutant")
  }
  
  
#print(xpart)
     
  


## produce the mean for the ID of the selectd Station

    mean(xpart, na.rm = TRUE)
}

Write a function that reads a directory full of files and reports the number of completely observed cases in each data file. The function should return a data frame where the first column is the name of the file and the second column is the number of complete cases. A prototype of this function follows
complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
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
		
		
		
}