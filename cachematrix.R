##
# Assignment: Caching the Inverse of a Matrix
# It has following two methods 
#
# 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#
# 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#    If the inverse has already been calculated (and the matrix has not changed), 
#    then the cachesolve should retrieve the inverse from the cache.
#

#
# makeCacheMatrix - This function creates a special "matrix" object that can cache its inverse.
#
makeCacheMatrix <- function(x = matrix()) {
  mat_inv <- NULL
  set <- function(y) {
    x <<- y
    mat_inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) mat_inv <<- inverse
  getInverse <- function() mat_inv
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


#
# cacheSolve - 
#
cacheSolve <- function(x, ...) {
  mat_inv <- x$getInverse()
  if (!is.null(mat_inv)) {
    message("getting cached data")
    return(mat_inv)
  }
  data <- x$get()
  mat_inv <- solve(data, ...)
  x$setInverse(mat_inv)
  mat_inv
}
