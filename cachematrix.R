## This file contains  2 function "makeCacheMatrix" which creates a special matrix for chaching
## and "cacheSolve" which calculates the Inverse of the matrix created in the earlier function.


## This function, makeCacheMatrix creates a special "matrix", which is a list containing a function
##set:set the value of the matrix
##get:get the value of the matrix
##setinverse: to set the value of inverse
##getinverse: to get the value of inverse
makeCacheMatrix <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse<- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## This function checks if the inverse of matrix has already been calculated.
## If so, it gets the inverse matrix from the cache and skips the computation. 
## Else, it calculates the inverse of the matrix and sets the value of the inverse in the cache
## via the setinverse function.
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
} 
