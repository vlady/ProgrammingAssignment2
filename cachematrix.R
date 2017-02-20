## Assignment: Caching the Inverse of a Matrix

## Creates a special "matrix" that also holds the inverse of it.
makeCacheMatrix <- function(mat = matrix()) {
  mat_inv <- NULL
  set <- function(new_mat) {
    mat <<- new_mat
    mat_inv <<- NULL
  }
  get <- function() mat
  setInverse <- function(matInv) mat_inv <<- matInv
  getInverse <- function() mat_inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Finds the inverse of a matrix and caches the results. If the inverse
## was already computed then the result is returned from cache for speed up.
cacheSolve <- function(mat, ...) {
  inv <- mat$getInverse()
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  data <- mat$get()
  inv <- solve(data, ...)
  mat$setInverse(inv)
  inv
}
