rankall <- function(outcome, num = "best") {
        ## Read outcome data and set global vars for data 
        md.result <- read.csv("./data/outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available", stringsAsFactors=FALSE)
        condition <- c("heart attack", "heart failure", "pneumonia")
        selst <- state
        result.split <- split(md.result, md.result$State)
        ## Check that state and outcome are valid
        if (is.element(state,md.result$State) == FALSE){
                stop ("invalid state")               
        }
        
        if(is.element(outcome,condition) == FALSE){
                stop("invalid outcome")
        }
        ## Asign case type var
        if (outcome == "heart attack"){
                case.type <- 11
                
        } else if (outcome == "heart failure"){
                case.type <- 17
        } else if (outcome == "pneumonia"){
                case.type <- 23
        }
        
        ## For each state, find the hospital of the given rank
        sapplY(result.split, function(x) min(x[, 11]))
        
        ## Return a data frame with the hospital names and the
        
        ## (abbreviated) state name
}

`