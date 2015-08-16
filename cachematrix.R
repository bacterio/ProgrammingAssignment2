## Here we are going to create two functions to cache the inverse of a matrix

## makeCacheMatrix will create a "special" type of matrix
## that will support "caching" of the "normal" and the
## inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve)
}


## This function will get a variable of the "special"
## type we created on the previous function. It will
## search for a cached value and return it if present.
## If not, it will call the solve function to get the
## inverse of the matrix, and cache the result afterwards

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if (!is.null(m)) {
        message('getting cached data')
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
