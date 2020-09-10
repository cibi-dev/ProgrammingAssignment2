# Matrix inversion is usually a costly computation and there may be 
# some benefit to caching the inverse of a matrix rather than compute
# it repeatedly (there are also alternatives to matrix inversion that 
# we will not discuss here). Your assignment is to write a pair of 
# functions that cache the inverse of a matrix.

# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <-NULL
    set <- function(a) {
        x <<- a
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(b) {
        inv <<- b
    }
    getInverse <- function() inv
    list(
        get = get,
        set = set,
        getInverse = getInverse,
        setInverse = setInverse
    )
}


# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and 
#the matrix has not changed), then the cachesolve should retrieve the 
# inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("Getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv
}
