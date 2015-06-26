## Reads the outcome-of-care-mesures.csv file provided
## take sthe state, condition for list and the requested rank to include
## "best" and "worst"  rankhospital

rankhospital <- function(state, outcome, num = "best") {
        
        ## Read outcome data
        md.result <- read.csv("./data/outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available", stringsAsFactors=FALSE)
        condition <- c("heart attack", "heart failure", "pneumonia")
        selst <- state
        qlimit <- "check"
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
        
       
        
        
        #extract only the requested state
        md.result.state <- md.result[md.result[,7] == state, ] # create a dataset of the state data
        
        # check for requested record number greater than data
        
        
        
        ## Set data type for columns
        md.result.state[,11] <- as.double(md.result.state[,11])
        md.result.state[,17] <- as.double(md.result.state[,17])
        md.result.state[,23] <- as.double(md.result.state[,23])
        
        ## asign num
        death.min <- min(md.result.state[,case.type], na.rm=TRUE)
        min.result <- md.result.state[md.result.state[,case.type] == death.min, ]
        death.max <- max(md.result.state[,case.type], na.rm=TRUE)
        max.result <- md.result.state[md.result.state[,case.type] == death.max, ]
        # remove records that contain NA in the column that is active
        
        clean.result <- subset(md.result.state,md.result.state[,case.type] != "NA")
                
        
        
        if (num == "best"){
                num <- 1
        } else if (num == 'worst'){
                num <- nrow(clean.result)
        }
        
        if (num > nrow(clean.result)){
                qlimit <- "over"
        }
                
        
        ## build matrix with sorted ranking of each hospital for the condition
        
        result.sort <- clean.result[order(clean.result[,case.type],clean.result$Hospital.Name),]
       
        result.rank <- cbind(result.sort$Hospital.Name, rank = (1:nrow(result.sort)))
        
        ## Return hospital name in that state with the given rank
        # 30-day death rate
        
        if (qlimit == "over"){
                return(NA)
        }else  result.rank[row=num,1]
}
