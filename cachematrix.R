## The following functions find and cache the value of the inverse of a square invertible matrix
## or retrieve the inverse from cahe if it has already been calculated for a given matrix

##  This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) { 
  v <- NULL
  set <- function(y) {
    x <<- y
    v <<- NULL
  }
  get <- function() x
  setinv <- function(solve) v <<- solve
  getinv <- function() v
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## retrieves the inverse from the cache.

cachesolve <- function(x, ...) {
  v <- x$getinv()
  if(!is.null(v)) {
    message("getting cached data")
    return(v)
  }
  data <- x$get()
  v <- solve(data, ...)
  x$setinv(v)
  v
}

