## These functions will calculate the inverse of a matrix and cache the results
## for reuse if the matrix has not changed

## Create a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        # cache variable 
        m <- NULL
        # set matrix, also clears the cache variable
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        # get the matrix
        get <- function() x
        # set the inverse value in cache
        setinverse <- function(inverse) m <<- inverse
        # get the inverse value from cache
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Compute the inverse of a special matrix returned by makeCacheMatrix.
## If the inverse has already been calculated and the matrix has not changed, 
## this function will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        # get cache value
        m <- x$getinverse()
        # if cache value found, return it instead of calculating inverse
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        # get the matrix
        data <- x$get()
        # calcualte the inverse
        m <- solve(data, ...)
        # cache the inverse value
        x$setinverse(m)
        ## Return a matrix that is the inverse of 'x'
        m
}
