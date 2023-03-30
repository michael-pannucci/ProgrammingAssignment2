## Coursera: R Programming (Week 3)
## Programming Assignment 2

## The makeCacheMatrix function creates a special matrix object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
    minv <- NULL
    set <- function(y) {
        x <<- y
        minv <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) minv <<- inverse
    get_inverse <- function() minv
    list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)
}


## The cacheSolve function computes the inverse of the special matrix returned
## by the makeCacheMatrix function. If the inverse has already been calculated,
## and the matrix has not been changed, then the cacheSolve function should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    minv <- x$get_inverse()
    if(!is.null(minv)) {
        message("getting cached data")
        return(minv)
    }
    matrix <- x$get()
    minv <- solve(matrix, ...)
    x$set_inverse(minv)
    minv
}
