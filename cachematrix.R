## These two functions allow for caching the inverse of a matrix
## so that it does not need to be recomputed if the matrix has
## not changed

## makeCacheMatrix creates an environment in which the matrix and
## its inverse are stored, and provides appropriate access functions

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## cacheSolve returns the inverse of the matrix stored in the
## makeCacheMatrix environment. It only calculates the inverse
## if it hasn't already been computed; otherwise it pulls the
## cached copy

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
  
}