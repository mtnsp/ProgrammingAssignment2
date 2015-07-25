## The following two functions are used to cache the inverse of a matrix.

## The makeCacheMatrix function creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinvrs <- function(invrs) i <<- invrs
  getinvrs <- function() i
  list(set = set, get = get,
       setinvrs = setinvrs,
       getinvrs = getinvrs)
}


## The cacheSolve function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the function retrieves 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinvrs()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  matrix <- x$get()
  i <- solve(matrix, ...)
  x$setinvrs(i)
  i    ## Return the inverse of 'x'
}

