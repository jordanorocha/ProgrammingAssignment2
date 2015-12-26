## The functions below basically compute the inverse of a matrix caching
## the result, i.e., saving the result and using it so that you don't have
## to do the time consuming calculation more than one time

## The makeCacheMatrix function creates a list of four functions: set(), get(),
## setinverse(), and getinverse(). The list is used as an argument to the
## following cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve function returns the inverse of matrix x, using the cached
## data if the inverse was already calculated

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
