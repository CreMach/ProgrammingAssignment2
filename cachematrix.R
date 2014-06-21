## This script enables user to create and store lists of matrices and their inverses
## and allow user to recall the inverses that already been stored.  

##This function creates an object that stores/sets a matrix and its inverse in cache.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function returns the inverse of matrix under makeCacheMatrix$get() from 
## cache if it exists, otherwise it will calculate the inverse.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m  ## Return a matrix that is the inverse of 'x'
}
