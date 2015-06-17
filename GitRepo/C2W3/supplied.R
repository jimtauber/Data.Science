## create a sample matrix
#mdata <- matrix(c(1:25), nrow = 5, ncol = 5)

#idata <- source(mdata)



## caching the mean of a vector as suppied by R class



makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                
                x <<- y
                m <<- NULL
                
        }
        get <- function() x
        setmean <- function(mean) m<<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
        
}

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        m <- mean(data,...)
        x$setmean(m)
        m
}