## Functions to create an object that stores matrix data, and keeps a
## cache of the inverse of the matrix so that it does not need to be
## calculated more than once

## makeCacheMatrix returns a list of functions to get and set both the
## matrix data itself and the inverse of the matrix

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


## cacheSolve returns the inverse of the matrix provided. The first
## time it is called, the inverse is calculated and cached. Subsequent
## calls simply retrieve the cached inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        ## inv is not NULL, so it is the cached inverse
        message("getting cached data")
        return(inv)
    }
    ## inv is NULL, so the inverse has not yet been calculated
    ## so get the matrix and calculate the inverse
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
