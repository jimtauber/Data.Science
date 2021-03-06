## md.result <- read.csv("./data/outcome-of-care-measures.csv", colClasses = "character")

best <- function(state, outcome) {
        ## Read outcome data read.csv includes flag to convert the string "Not Available" to R type NA
        md.result <- read.csv("./data/outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available", stringsAsFactors=FALSE)
        condition <- c("heart attack", "heart failure", "pneumonia")
        selst <- state
        ## Check that state and outcome are valid
        if (is.element(state,md.result$State) == FALSE){
                stop ("invalid state")               
        }
        
        if(is.element(outcome,condition) == FALSE){
                stop("invalid outcome")
        }
        ## Asign case type var - allows for direct selection of column. option would be to rename the column for better access
        if (outcome == "heart attack"){
                case.type <- 11
                
        } else if (outcome == "heart failure"){
                case.type <- 17
        } else if (outcome == "pneumonia"){
                case.type <- 23
        }
        
        
        ## Return hospital name in that state with lowest 30-day death
        ## for case type
        md.result.state <- md.result[md.result[,7] == state, ] # create a dataset of the state data
        ## convert rates to numeric, cleaning up for proper sorting
        md.result.state[,11] <- as.double(md.result.state[,11])
        md.result.state[,17] <- as.double(md.result.state[,17])
        md.result.state[,23] <- as.double(md.result.state[,23])
        
                     
        
        death.min <- min(md.result.state[,case.type], na.rm=TRUE)
        min.result <- md.result.state[md.result.state[,case.type] == death.min, ]
        death.max <- max(md.result.state[,case.type], na.rm=TRUE)
        max.result <- md.result.state[md.result.state[,case.type] == death.max, ]
        
        # need to sort list by hospital name
        result.sort <- min.result[order(min.result$Hospital.Name),]
        
        return(result.sort[1,2])
        
}


