## A set of functions to store a matrix, calculate its inverse and
## cache the results of that calculation.

## Takes a matrix and returns an object which stores a matrix and 
## manages a cache containing its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                if(!identical(x,y)) {
                        x <<- y
                        message("Clearing cache")
                        m <<- NULL
                }
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Takes a makeCacheMatrix object as input and computes the inverse 
## of the matrix or returns previously calculated inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}
