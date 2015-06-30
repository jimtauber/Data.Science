rankall <- function(outcome, num = "best") {
        
        ##outcome <- "heart attack"
        ##num = "best"
        ## Read outcome data and set global vars for data 
        md.result <- read.csv("./data/outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available", stringsAsFactors=FALSE)
        condition <- c("heart attack", "heart failure", "pneumonia")
        num <- num
        reduced.result <- md.result[,c(2,7,11,17,23)]
        names(reduced.result) <- c("hospital", "state", "out.attack", "out.faulure", "out.pneumonia")    
        
        ## Check that outcome is valid
                
        if(is.element(outcome,condition) == FALSE){
                stop("invalid outcome")
        }
        ## Asign case type var
        if (outcome == "heart attack"){
                case.type <- 3
                
        } else if (outcome == "heart failure"){
                case.type <- 4
        } else if (outcome == "pneumonia"){
                case.type <- 5
        }
        
        ## set the data type for results
        reduced.result[,3] <- as.numeric(reduced.result[,3])
        reduced.result[,4] <- as.numeric(reduced.result[,4])
        reduced.result[,5] <- as.numeric(reduced.result[,5])
        
        # Function to determin the hospital rank
        
        clean.result <- subset(reduced.result,reduced.result[,case.type] != "NA")
        
        #rankdata <- function(x){
         #       result.sort <- clean.result[order(clean.result[,case.type],clean.result$Hospital.Name),]
            #    result.rank <- cbind(result.sort$Hospital.Name, rank = (1:nrow(result.sort))) 
        #}
        
        #result.sort <- clean.result[order(clean.result[,case.type],clean.result$Hospital.Name),]
        
        #result.rank <- cbind(result.sort$Hospital.Name, rank = (1:nrow(result.sort)))
        
        ## For each state, find the hospital of the given rank
        result.split <-split(clean.result, clean.result$State)
        result.ordered <- lapply(result.split, function(x) { x[order(x[,case.type]),]})
        
        #assign numeric value for num
        stlen<-do.call(cbind,lapply(result.ordered, nrow))
        if (num == "best"){
                num <- 1
        } else if (num == 'worst'){
                num <- nrow(result.ordered)
        }
        
        #if (num > nrow(clean.result)){
         #       qlimit <- "over"
        #}
        ## Return a data frame with the hospital name and the
        ## (abbreviated) state name
        my_out <- do.call(rbind,lapply(result.ordered, function(x) {x[num,1:2] }))
        
        #stlen<-do.call(cbind,lapply(result.ordered, nrow))
        
        
        my_out
        
}


