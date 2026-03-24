## These functions create a special matrix object that can cache its inverse.
## If the inverse has already been computed, it can be returned without recalculating it.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## Set the matrix value and reset the cached inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## Return the current matrix
  get <- function() x
  
  ## Store the inverse in the cache
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  ## Return the cached inverse
  getInverse <- function() inv
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function returns the inverse of the special matrix.
## If the inverse has already been cached, it returns the cached value.
## Otherwise, it computes the inverse, stores it, and returns it.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
