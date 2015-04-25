## The makeCacheMatrix function creates a special "matrix" object containing a matrix and its inverse.
## The cacheSolve function returns the inverse of a "matrix" that is constructed with a call to makeCacheMatrix. 
## If M is a matrix constructed with makeCacheMatrix(x), a first call of cacheSolve(M) effectively computes the 
## inverse of the matrix x. The inverse of x is cached in M for all subsequent calls of cacheSolve(M). 
## This returned value of cacheSolve(M) will be correct as long as the matrix x remains unmodified.

## makeCacheMatrix
## ---------------
## Function that converts a matrix argument x into an augmented representation containing the inverse of x.
## Returns a list object containing getter and setter functions for the given matrix x and its inverse.
makeCacheMatrix <- function(x = matrix()) {
     inverse <- NULL
     set <- function(y) {
          x <<- y
          inverse <<- NULL
     }
     get <- function() x
     setInverse <- function(inverseMatrix) inverse <<- inverseMatrix
     getInverse <- function() inverse
     list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve
## ----------
## Function that calculates the inverse of an augmented representation x of an (invertible) matrix.
## Optional arguments are passed unmodified to an internal call to the solve function.
## Returns the inverse of the given matrix.
cacheSolve <- function(x, ...) {
     inverse <- x$getInverse()
     if (!is.null(inverse)) {
          message("Returning cached data")
          return(inverse)
     }
     matrix <- x$get()
     inverse <- solve(matrix, ...)
     x$setInverse(inverse)
     inverse
}
