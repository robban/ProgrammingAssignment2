## A set of functions to create a matrix with a cached value of the inverse
## Use the function cacheSolve to use the cached value if it exists

## Make a matrix with a function to cache the inverse value
makeCacheMatrix <- function(x = matrix()) {
  cached_inverse <- NULL
  set <- function(y) {
    x <<- y
    cached_inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) cached_inverse <<- inverse
  getinverse <- function() cached_inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Returns a matrix that is the inverse of 'x',
## using the cached value in x if available
## Note: x should be an object created with makeCacheMatrix, it can not be an ordinary matrix

cacheSolve <- function(x, ...) {
  cached_inverse <- x$getinverse()
  if(!is.null(cached_inverse)) {
    message("getting cached data")
    return(cached_inverse)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
