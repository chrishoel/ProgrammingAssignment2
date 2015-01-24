## Coursera: R Programming -- Programming Assignment #2
## These two functions work together to calculate the inverse of a given matrix. Additionally, this inverse is cached. Matrix
## inversion is computationally heavy, and this cache function helps to alleviate that burden.
## 
## These functions are adapted from the course examples for calculating and caching the mean of a vector. It assumes that the given
## matrix is always invertable.
## 
##

## -------

## makeCacheMatrix creates a "matrix object" which is capable of caching it's inverse. This "matrix object" can then be passed to
## cacheSolve in order to either calculate the inverse, or retrieve it.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix)  inv <<- matrix
  getmatrix <- function() inv
  list(set=set,get=get,setmatrix=setmatrix,getmatrix=getmatrix)
}


## cacheSolve gets passed a "matrix object" resulting from makeCacheMatrix. cacheSolve then either computes and caches the inverse 
## of the given matrix, or retrieves the cached inverse if one has already been computed.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getmatrix()
  if (!is.null(inv)) {
      message("Getting cached inverse:")
      return(inv)
  }
  dat <- x$get()
  inv <- solve(dat,...)
  x$setmatrix(inv)
  inv
}

## Example of code in use:
## > examplematrix <- matrix(3:6,2,2)
## > examplematrix
##     [,1] [,2]
## [1,]  3    5
## [2,]  4    6
## > solve(examplematrix)
##     [,1] [,2]
## [1,] -3  2.5
## [2,]  2 -1.5
## > examplematobj <- makeCacheMatrix(examplematrix)
## > cacheSolve(examplematobj)
##     [,1] [,2]
## [1,] -3  2.5
## [2,]  2 -1.5
## > cacheSolve(examplematobj)
## Getting cached inverse:
##     [,1] [,2]
## [1,] -3  2.5
## [2,]  2 -1.5
