##
##


# Caches the inverse of a matrix
# 
# It creates a special vector, which is really a list containing a function to:
# - set the value of the matrix
# - get the value of the matrix
# - set the value of the inverse
# - get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# Computes the inverse of matrix
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
