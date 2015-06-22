## This file contains code for the Coursera R Programming class,
## specifically the second programming assignment.
## This implementation is a caching mechanism for the inverse
## of matricies in order to avoid recomputing the same
## computations.

# makeCacheMatrix is a funciton that generates an R object impleted 
# as a list of functions and a state.  The functions allow the user 
# to get and set the internal state of the object.  The state is a 
# matrix and that matrix's value. These functions and states are used
# in determining whether the inverse has already been
# computed and to store and retrieve a cached copy of that inverse.

makeCacheMatrix <- function(x = matrix()) {
    # internal local cached inverse value
    inv <- NULL
    
    # initialize the internal matrix
    set <- function (y) {
        x <<- y
        inv <<- NULL
    }
    # return the internal matrix
    get <- function () {
        x
    }
    # set the local inverse value
    setInverse <- function (z) {
        inv <<- z
    }
    # get the local inverse value
    getInverse <- function () {
        inv
    }
    list(
        set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse
    )
}


# cacheSolve is a function that takes in a cached matrix object and 
# returns its inverse.  It does this by first checking the object's
# cache.  If the inverse has alredy been calculated, that inverse
# is returned.  If the inverse has not been calculated, it is
# calculated, cached into the object for future use, and returned.

cacheSolve <- function(x, ...) {
    
    inv <- x$getInverse()
    
    # returned the cached inverse value if it exists
    if (!is.null(inv)) {        
        message("returning cached value")
        return(inv)
    }
    
    # if inverse does not exist, calculate it and cache it
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}