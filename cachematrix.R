## Matrix inversion is usually a costly computation and there may 
## be some benefit to caching the inverse of a matrix rather than compute it repeatedly

## This function creates "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL
  set <- function(y) {
    x <<- y
    invm <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invm <<- inverse
  getInverse <- function() invm
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invm <- x$getInverse()
  if (!is.null(invm)) {
    message("getting cached data")
    return(invm)
  }
  data <- x$get()
  invm <- solve(data, ...)
  x$setInverse(invm)
  invm
}
