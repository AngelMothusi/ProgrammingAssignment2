## These functions two functions can store their own inverse.
## Caching this prevents you from having to recalculate it 
## if the matrix hasn't changed.

## makeCacheMatrix: This function creates a "matrix" object, 
## which is a list of functions to get the matrix and 
## its cached inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve: This function computes the inverse of the "matrix" 
## returned by makeCacheMatrix. It first checks if the inverse has 
## already been calculated. If so, it gets it from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}