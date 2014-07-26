## Functions for creating an object containing a matrix and its inverse,
## solving for the inverse, and caching the inverse to ease computation.


## makeCacheMatrix creates an object that stores a matrix and its inverse,
## with methods for getting and setting both.

## x$set() stores new matrix.
## x$get() retrieves the stored matrix.
## x$setinv() stores the matrix's inverse.
## x$getinv() retrives the stored inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                              ## On creation, set inverse to null.
        set <- function(y) {                   ## Function for changing the matrix.
                x <<- y                        ## Stores new matrix.
                m <<- NULL                     ## Erases stored inverse.
        }
        get <- function() x                    ## Function to retrieve stored matrix.
        setinv <- function(solve) m <<- solve  ## Function to set stored inverse.
        getinv <- function() m                 ## Function to retrieve stored inverse.
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve takes object created by makeCacheMatrix, solves for and
## caches its inverse, or returns the cached inverse if available.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {                       ## If there is a cached inverse...
                message("getting cached data")  ## announce that...
                return(m)                       ## and return it.
        }
        data <- x$get()                         ## Otherwise, get the matrix...
        m <- solve(data, ...)                   ## invert it...
        x$setinv(m)                             ## cache the inverse...
        m                                       ## and return it.
}
