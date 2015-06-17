## This function is designed to create a cached copy of
## the inverse of a 'special' matrix object. It was created for Week 3
## of the Programing R coursera class.

## Function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
       m <- NULL
       set <- function(y) {
               x <<- y
               m <<- NULL
       }
       get <- function() x
       
       setsol <- function(solve) m <<- solve
       
       getsol <- function() m
       
       list (set = set, get = get, setsol = setsol, getsol = getsol)
}


## Function is designed to read the  output from makeCasheMatrix()
## determine if the inverse is already in memory, confirm the matris has not changed
## and either use the existing or compute the inverse. It will also store the result in 
## memory if needed.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
        
        ## asign the function stored at x$getsol to the var m
        m <- x$getsol() 
        
        ## check to confirm m is not (!) null indicating that the cached data already exists
        ## and return it, exiting the function
        if(!is.null(m)) {
                
                message("getting cached data")
                
                return(m)
                
        }
        
        # asign the function stored at x$get to data
        data <- x$get()
        
        # run the solve function over the output of data
        m <- solve(data, ...)
        
        
        x$setsol(m)
        
        ## return the value of m
        m
        
}


