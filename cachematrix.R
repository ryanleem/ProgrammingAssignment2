makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setmean <- function(solve) {
    m <<- solve
  }
  
  getmean <- function() {
    m
  }
  
  list(
    set = set,
    get = get,
    setmean = setmean,
    getmean = getmean
  )
}

cacheSolve <- function(x, ...) {
  m <- x$getmean()
  
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setmean(m)
  m
}

m <- makeCacheMatrix(matrix(c(2, 1, 1, 2), 2, 2))
cacheSolve(m)
cacheSolve(m)