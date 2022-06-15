## To Cache the Inverse of a Matrix


## makeCacheMatrix creates the list of several functions to: 1) Set the value of the vector, 
## 2) Get the value of the vector, 3) Set the value of the mean, 4) Get the value of the mean.

        makeCacheMatrix <- function(x = matrix()) {
                                        inv <- NULL
                                        set <- function(y) {
                                        x <<- y
                                        inv <<- NULL
  }
                                        get <- function() x
                                        setInverse <- function() inv <<- solve(x)
                                        getInverse <- function() inv
                                        list(set = set,
                                        get = get,
                                        setInverse = setInverse,
                                        getInverse = getInverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by the function above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
                                inv <- x$getInverse()
                                if (!is.null(inv)) {
                                message("getting cached data")
                                return(inv)
    }
                                mat <- x$get()
                                inv <- solve(mat, ...)
                                x$setInverse(inv)
                                inv
}
