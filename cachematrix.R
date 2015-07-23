## cacheMatrix.R
## Contains functions to create and cache the inverse of a matrix.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## It contains functions to:
## 1. set the value of the matrix
## 2. get the vale of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve computes the inverse of the special
## "matrix returned by makeCacheMatrix. If the
## inverse has already been calculated (and the
## matrix has not changed), then the cachesolver
## should return the inverse from the cache.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
