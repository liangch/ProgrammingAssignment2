# Matrix inversion is usually a costly computation and their may be some benefit 
# to caching the inverse of a matrix rather than compute it repeatedly 

makeCacheMatrix <- function(x = matrix()) {
    # This function creates a special "matrix" object that can cache its inverse.
    i <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
    # This function computes the inverse of the special "matrix" returned by 
    # makeCacheMatrix above. If the inverse has already been calculated (and the
    # matrix has not changed), then the cachesolve should retrieve the inverse from
    # the cache.
    i <- x$getinverse()
    if (!is.null(i)) {
        message("gettting cached data")
        return(i)
    }
    
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
