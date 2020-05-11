## These functions are used to cache (storing data locally for quick later retrieval)
## the inverse of a matrix. These functions will store the matrix and cache its inverse.

## Creates a matrix object that will be use to cache its inverse:
makeCacheMatrix <- function(x = matrix()) {
  v <- NULL
  set <- function(y) {
    x <<- y
    v <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) v <<- inverse
  getInverse <- function() v
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Returns the inverse matrix of x. If the inverse of x has already calculated, then the 
## cached inverse will be returned:

cacheSolve <- function(x, ...) {
  v <- x$getInverse()
  if (!is.null(v)) {
    message("getting cached data")
    return(v)
  }
  mtx <- x$get()
  v <- solve(mtx, ...)
  x$setInverse(v)
  v
}