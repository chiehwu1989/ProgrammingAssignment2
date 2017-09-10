## This script provides functions to calculate the inverse of a Matrix and cache the result
## for repeated use

## Creates a speical "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    # Validate parameter data type
    if(!is.matrix(x)) {
        stop("'x' is not a matrix")
    }

    # Initialize variable for cached inverse matrix
    cacheInvMatrix <<- NULL

    # Sets matrix
    set <- function(y) {
        x <<- y
        cacheInvMatrix <<- NULL
    }

    # Gets matrix
    get <- function() x

    # Sets cached inverse matrix
    setInvMatrix <- function(invMatrix) cacheInvMatrix <<- invMatrix

    # Gets cached inverse matrix
    getInvMatrix <- function() cacheInvMatrix

    list(set = set, get = get, setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)
}


## Computes and caches the inverse of the matrix object returned by 'makeCacheMatrix'
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInvMatrix()

    if(!is.null(m)) {
        # The given object has cached result
        message("Getting result from cached data")
        return(m)
    }

    data <- x$get()

    # Calculate inverse of the matrix
    m <- solve(data, ...)
    # Update cache
    x$setInvMatrix(m)

    m
}
