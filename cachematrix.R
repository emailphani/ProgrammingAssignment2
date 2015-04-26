## function cacheSolve() calculates the reverse of the matrix created with the makeCacheMatrix() 
## function. However, it first checks to see if the reverse has already been calculated. 
## If so, it gets the mean from the cache and skips the computation. 
## Otherwise, it calculates the matrix reverse and sets the value of the reverse 
## in the cache via the setmean function.
## 

## Short comment describing this function
## Author: Phani Tipparaju
## Date Authored: 24th April 2015
## This function creates a special "matrix" object that can cache its inverse.
##

makeCacheMatrix <- function(x = matrix()) {
 
        rev <- NULL
        
        set <- function(y) {
                x <<- y
                rev <<- NULL
        }
        
        get <- function() x
        
        setReverse <- function(inverse) rev <<- inverse
        
        getReverse <- function() rev
        
        list(set = set, get = get,
             setReverse = setReverse,
             getReverse = getReverse)        
}


## Return the matrix inverse from cache if available

cacheSolve <- function(x,...) {
        ## Return a matrix that is the inverse of 'x'
        
        reverse <- x$getReverse()
        
        ## Check in the cache if reverse is already available
        ## if yes, then return the value from cache        
        if(!is.null(reverse)) {
                message("getting cached data")
                return(reverse)
        }
        
        data <- x$get()
        
        ## this is where reverse is calculated
        reverse <- solve(data, ...)
        
        ## this is where reverse is populated into the object
        x$setReverse(reverse)
        
        reverse
}
