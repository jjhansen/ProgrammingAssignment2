## R Programming 
## Assignment 2 
## Computing the inverse of a matrix can be time consuming.  These
## functions create a matrix that can cache its inverse and 
## return the value of the cached inverse.  

## This function creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    # set the value of the matrix.  Clear the cached inverse
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # return the matrix 
    get <- function() x
    # set the inverse
    setinverse <- function(solve) i <<- solve
    # get the inverse of the matrix x
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of a special matrix and caches
## it.  If the matrix has already been solved, it returns the cached 
## value.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
