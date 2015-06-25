## Reads the outcome-of-care-mesures.csv file provided
## take sthe state, condition for list and the requested rank to include
## "best" and "worst"  rankhospital

rankhospital <- function(state, outcome, num = "best") {
        
        ## Read outcome data
        md.result <- read.csv("./data/outcome-of-care-measures.csv", colClasses = "character")
        condition <- c("heart attack", "heart failure", "pneumonia")
        selst <- state
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
                ## md.result.stsel[,12] <- as.double(md.result.stsel[,12])
                ## bad <- is.na(md.result.stsel[case.type])
        } else if (outcome == "heart failure"){
                case.type <- 17
        } else if (outcome == "pneumonia"){
                case.type <- 23
        }
        
       
        #test data
        state <- "NC"
        case.type <- 11
        
        #extract only the requested state
        md.result.state <- md.result[md.result[,7] == state, ] # create a dataset of the state data
        ## Set data type for columns
        md.result.state[,11] <- as.double(md.result.state[,11])
        md.result.state[,17] <- as.double(md.result.state[,17])
        md.result.state[,23] <- as.double(md.result.state[,23])
        
        ## asign num
        if (num == "best"){
                num <- 1
        } else if (num == 'worst'){
                num <- nrow(md.result.state)
        }
        ## build matrix with sorted ranking of each hospital for the condition
        #result.sort <- md.result.state[order(md.result.state[,case.type]),]
        result.sort <- md.result.state[order(md.result.state[,case.type],md.result.state$Hospital.Name),]
        ## df[order(df[,1],df[,2],decreasing=TRUE),]
        result.rank <- cbind(result.sort$Hospital.Name, rank = (1:nrow(result.sort)))
        ## Return hospital name in that state with the given rank
        # 30-day death rate
        result.rank[row=num,1]
}
