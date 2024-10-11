## set the environment 

install.packages("matlib")
library(matlib)

## Put comments here that give an overall description of what your
## functions do

## This function takes a matrix and gives it a structure where its inverse can
## be cached

makeCacheMatrix <- function(x = matrix()) {
  inv.mat <- NULL
  set <- function(y){
    x <<- y
    inv.mat <<- NULL
  }
  get <- function() x
  setinv <- function(inverse_matrix) inv.mat <<- inverse_matrix
  getinv <- function() inv.mat
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This Function takes a specialized list object returned from "makeCacheMatrix" 
## function and returns its inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv.mat <- x$getinv()
  if (!is.null(inv.mat)){
    message("getting cached data")
    return(inv.mat)
  }
  data <- x$get()
  inv.mat <- inv(data, ...)
  x$setinv(inv.mat)
  inv.mat
}
