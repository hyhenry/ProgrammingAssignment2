## makeCacheMatrix and cacheSolve are used in conjunction to solve and
## cache the inverse of a matrix

## makeCacheMatrix is a special matrix object that returns a list of
## functions to get/set the matrix, and get/set the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverseVal) inv <<- inverseVal
  getInverse <- function() inv
  list (set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## cacheSolve takes the matrix object from makeCacheMatrix() and 
## returns the matrix inverse. If the inverse has been cached it will
## return the cached value, otherwise it will compute, cache, and then
## return the inverse

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}
