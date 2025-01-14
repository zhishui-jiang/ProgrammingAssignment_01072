## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## function makeCacheMatrix to create special matrix object

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL  ## Initialize the cache for the inverse as NULL.
        
        ## Function to set the value of the matrix.
        set <- function(y) {
                x <<- y  ## Set the matrix value.
                m <<- NULL  ## Reset the cached inverse to NULL when the matrix changes.
        }
        
        ## Function to get the current value of the matrix.
        get <- function() x 
        
        ## Function to set the cached inverse of the matrix.
        setInverse <- function(inverse) m <<- inverse
        
        ## Function to get the cached inverse of the matrix.
        getInverse <- function() m
        
        ## Return a list of all functions
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## function cacheSolve to check if matrix inverse has already been calculated,
## if not, calculate the inverse

cacheSolve <- function(x, ...) {
        m <- x$getInverse()  ## Check if the inverse is already cached.
        
	## If cached, return the cached inverse.
        if(!is.null(m)) {
                message("getting cached data")  
                return(m)
        }
        
	## Get the matrix from the object.
        data <- x$get()  
	## Compute the inverse of the matrix.
        m <- solve(data, ...)  
	## Cache the computed inverse.
        x$setInverse(m)  
	## Return the computed inverse.
        m  
}



## Create a 2x2 matrix
#my_matrix <- matrix(c(4, 7, 2, 6), nrow = 2)

## Create a special matrix object and cache it
#cached_matrix <- makeCacheMatrix(my_matrix)

## Compute and cache the inverse of the matrix
#inverse_matrix_1 <- cacheSolve(cached_matrix)
#print("First calculation (inverse matrix):")
#print(inverse_matrix_1)

## Calling cacheSolve again should fetch the inverse matrix from the cache 
## instead of recalculating it
#inverse_matrix_2 <- cacheSolve(cached_matrix)
#print("Second calculation (cached inverse matrix):")
#print(inverse_matrix_2)