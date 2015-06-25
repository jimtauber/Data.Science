## md.result <- read.csv("./data/outcome-of-care-measures.csv", colClasses = "character")

best <- function(state, outcome) {
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
                case.type <- 12
        } else if (outcome == "heart failure"){
                case.type <- 17
        } else if (outcome == "pneumonia"){
                case.type <- 23
        }
        ## Return hospital name in that state with lowest 30-day death
        ## for heartattack
        md.result.stsel <- md.result[md.result[,7] == state, ] # create a dataset of the state data
        death.min <- min(md.result.stsel[,case.type])
        min.result <- md.result.stsel[md.result.stsel[,case.type] == death.min, ]
        
        # need to sort list by hospital name
        result.sort <- min.result[order(min.result$Hospital.Name),]
        
        return(result.sort[,2])
        
}


