## This function creates a "matrix" object that can cache its inverse

makeCacheMatrix <- function(y = matrix()) {
inv <- NULL
  set <- function(z) {
          y <<- z
          inv <<- NULL
  }
  get <- function() y
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##The inverse of the "matrix" returned by the previous function is computed with this function.
##If the inverse has already been calculated and the matrix has not changed, use this code.
## Then the inverse from the cache should be retrieved by the cachesolve


cacheSolve <- function(y, ...) {
       inv <- y$getinverse()
  if (!is.null(inv)) {
          message("getting cached data")
          return(inv)
  }
  data <- y$get()
  inv <- solve(data, ...)
  y$setinverse(inv)
  inv
}
