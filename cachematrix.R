## This file contains the functions makeCacheMatrix and cacheSolve
## which create a special matrix object in the form of a list of 
## functions to get and set matrix data and calculate the matrix 
## inverse.


## makeCacheMatrix - creates a special matrix object
## that can cache its inverse.
## Arguments:
##   * x (matrix): the matrix data. The default value 
##        is an empty matrix.
##
## Returns:
##   A list containing the functions set, get, setinv and getinv
##   which set/get the matrix data and the matrix inverse respectively.

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setinv <- function(i) inv <<- i
     getinv <- function() inv
     list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve - calculates and stores the inverse of a matrix object
## created using makeCacheMatrix. If the inverse has already
## been calculated and the matrix data has not changed, it
## returns the cached inverse.
##
## Arguments:
##   * x (matrix object): the matrix object as created using
##        makeCacheMatrix. Additional arguments are passed 
##        to solve.
##
## Returns:
##   The matrix inverse.

cacheSolve <- function(x, ...) {
     i <- x$getinv()
     
     if (!is.null(i)) {
          message("Getting cached inverse")
          return(i)
     }
     mat <- x$get()
     i <- solve(mat, ...)
     x$setinv(i)
     i
}
