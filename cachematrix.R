## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL  ## Initialize the cache for the inverse as NULL.
        
        set <- function(y) {
                x <<- y  ## Set the matrix value.
                m <<- NULL  ## Reset the cached inverse to NULL when the matrix changes.
        }
        
        get <- function() x 
        
        setInverse <- function(inverse) m <<- inverse
        
        getInverse <- function() m
        
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        m <- x$getInverse()          
        if(!is.null(m)) {
                message("getting cached data")  
                return(m)
        }
        
        data <- x$get()  
        m <- solve(data, ...)  
        x$setInverse(m)  
        m  
}