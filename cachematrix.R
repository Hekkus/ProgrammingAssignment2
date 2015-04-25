## Below are two functions that are used to create a special object that stores
## a matrix and cache's its inverse

## Function that stores a list of functions to set and get the value of a matrix
## and its inverse
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

## Function that calculates the inverse of the matrix created with the above
## funciont and caches it. If the inverse is already cached it skips the
## computation and simply returns the value.
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        ## Return a matrix that is the inverse of 'x'
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

