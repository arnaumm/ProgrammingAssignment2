## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than computing it repeatedly.
## In this file you will find two functions:
## makeCacheMatrix that will create a special matrix object.
## cacheSolve that will compute the inverse of the special matrix created with makeCacheMatrix.

## makeCacheMatrix creates a special matrix object that can cache it's inverse.
## Some functions will be available for this matrix object:
## set: Sets a new matrix and resets any previous cached inverse
## get: Retrieves the matrix
## set_inverse: Sets the inverse matrix to be cached
## get_inverse: Retrieves the cached inverse

makeCacheMatrix <- function(x = matrix()) {
 
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) i <<- inverse
    get_inverse <- function() i
    list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
    
}


## cacheSolve computes the inverse of the special matrix returned by 
## makeCacheMatrix. 
## If the inverse has already been calculated and the matrix has not changed,
## then cacheSolve retrieves the inverse from the cache. 
## If it has not been calculated, or the matrix has changed, it calculates
## the inverse and then stores the result in the cache.

cacheSolve <- function(x, ...) {
    
    i <- x$get_inverse()
    if (!is.null(i)) {
        message('Getting cached inverse')
        return(i) 
    }
    data <- x$get()
    i <- solve(data, ...)
    x$set_inverse(i)
    i   
    
}