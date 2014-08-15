## The following functions calculate the inverse of a matrix and then stores that
## in cache so that if the inverse calculation has to be used again, it already 
## exists in memory and does not have to be redone

## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x<<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<-solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the invefse of the matrix created in makeCacheMatrix. If it has 
## already been calculated, then it retrieves the inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
