##cachesolve pulls from a cached value if available (and if the available data has not changed 
##since the last time run), and if not finds the inverse of the matrix created by makeCacheMatrix
##and caches its value.

##Saves a "matrix" object with the ability to cache inverse

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

##Finds inverse of object created with makeCacheMatrix; uses cached value if already cached 
##and no values have changed

cachesolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}