##  The functions are used to create an R object 
##that stores a matrix and cache's its inverse. 

## The function creates a matrix and returns a list 
## of functions that set :
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y 
    inverse <<- NULL
  }
  get <- function() x
  set_inverse <- function(solve) inverse <<- solve
  get_inverse <- function() inverse
  list(set = set, get = get, 
       set_inverse = set_inverse, get_inverse = get_inverse)
  
}

## Returns the inverse of the matrix created with makeCacheMatrix
## by calculating it, or by returning the value stored in cache, 
## if the inverse has previously been calculated

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$get_inverse()
  if(!is.null(inverse)) {
    message(("getting cached data"))
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$set_inverse(inverse)
  inverse
}