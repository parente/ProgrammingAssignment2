## Defines functions that compute and cache the inverse of a matrix to improve
## performance when the inverse is needed multiple times.

## Wraps the given matrix to enable the caching of its inverse. Returns a 
## list with the ability to get/set the matrix and get/set its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL

    ## Set a new matrix, and be sure to reset the
    ## cached inverse
    set <- function(y) {
        x <<- y
        inv <- NULL
    }

    ## Return the current matrix
    get <- function() x

    ## Cache the computed inverse
    setInverse <- function(i) {
        inv <<- i
    }

    ## Return the cached inverse
    getInverse <- function() inv

    ## Return a list with the functions as properties
    list(
        set=set, 
        get=get,
        setInverse=setInverse,
        getInverse=getInverse
    )
}

## Finds the inverse of a matrix constructed by makeCacheMatrix. Caches the
## inverse and returns it. On repeat invocations, returns the cached value
## without solving for the inverse again.

cacheSolve <- function(x, ...) {
    ## Check if the inverse has been computed and cached
    inv <- x$getInverse()

    if(!is.null(inv)) {
        ## If so, return it without recomputing it
        return(inv)
    }

    ## Fetch the matrix from its wrapping
    m <- x$get()

    ## Compute the inverse of the given matrix, passing through any additional
    ## parameters given to the cacheSolve function
    inv <- solve(m, ...)

    ## Cache the inverse to avoid recomputing it next time
    x$setInverse(inv)

    ## Return the inverted matrix
    inv
}
