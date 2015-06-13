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