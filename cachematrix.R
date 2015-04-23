## Two functions that create a 'matrix' object and caches the 
## inverse of said matrix

## This function creates a 'matrix'object that exposes get, set methods
## for the matrix and get, set methods for the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        # set the matrix data
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # get the matrix data
        get <- function() {x}
        
        # set the cached inverse matrix
        setsolve <- function(solve) {inv <<- solve}
        # get the cached inverse matrix
        getsolve <- function() {inv}
        
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## A Function to find the inverse of a matrix and cache it in a 'matrix'
## object created with the above function

cacheSolve <- function(x, ...) {
        inv <- x$getsolve()
        # check if value is already cached
        if(!is.null(inv)) {
                # if cached return cached value rather than recalculate
                message("getting cached data")
                return(inv)
        }
        # get the stored matrix
        data <- x$get()
        # find inverse
        inv <- solve(data, ...)
        # set the cached inverse = calculated inverse
        x$setsolve(inv)
	# return inverse
        inv
}
