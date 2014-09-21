## This file contains a set of functions for working with matrices that are
## expensive to solve by caching the matrix's inverse.

## Convert a matrix into a special object that contains the matrix data and
## the ability to cache the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
            x <<- y
            inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Solve a matrix created by the makeCacheMatrix function. The inverse is
## cached by the function to allow for fast calculation when called repeatedly.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
