## Caching the Inverse of a Matrix
## A pair of functions that cache the inverse of a matrix.
## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly.

## The first function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    iv <- NULL
    set <- function(y) {
        x <<- y
        iv <<- null
    }    
    get <- function() x
    setinverse <- function(solve) iv <<- solve
    getinverse <- function() iv
    list(set = set, get = get, 
    setinverse = setinverse, 
    getinverse = getinverse)
    
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        iv <- x$getinverse()
        if(!is.null(iv)) {
            message("getting cached data")
            return(iv)
        }
        data <- x$get()
        iv <- solve(data, ...)
        x$setinverse(iv)
        iv
}
